use proptest::prelude::*;

#[derive(Clone, Debug)]
struct Model {
    deposited: i128,
    withdrawn: i128,
    rate: i128,
    last_time: u64,
    paused_at: Option<u64>,
}

impl Model {
    fn claimable(&self, now: u64) -> i128 {
        let end = self.paused_at.unwrap_or(now);
        let elapsed = end.saturating_sub(self.last_time) as i128;
        (elapsed.saturating_mul(self.rate))
            .min(self.deposited.saturating_sub(self.withdrawn).max(0))
    }
}

#[derive(Clone, Debug)]
enum Action {
    Advance(u32),
    Pause,
    Resume,
    Withdraw,
    TopUp(u32),
    Cancel,
}

fn action_strategy() -> impl Strategy<Value = Action> {
    prop_oneof![
        (1u32..10_000).prop_map(Action::Advance),
        Just(Action::Pause),
        Just(Action::Resume),
        Just(Action::Withdraw),
        (1u32..100_000).prop_map(Action::TopUp),
        Just(Action::Cancel),
    ]
}

proptest! {
    #![proptest_config(ProptestConfig { cases: 10_000, .. ProptestConfig::default() })]
    #[test]
    fn randomized_stream_invariants(
        deposited in 1i128..1_000_000_000_000_000_000i128,
        duration in 1u64..100_000u64,
        actions in prop::collection::vec(action_strategy(), 1..100),
    ) {
        let mut model = Model { deposited, withdrawn: 0, rate: (deposited / duration as i128).max(1), last_time: 0, paused_at: None };
        let mut now = 0u64;
        let mut previous_withdrawn = 0i128;
        let mut paused_claimable = None;
        let mut cancelled = false;

        for action in actions {
            match action {
                Action::Advance(dt) if !cancelled => now = now.saturating_add(dt as u64),
                Action::Pause if !cancelled && model.paused_at.is_none() => {
                    paused_claimable = Some(model.claimable(now));
                    model.paused_at = Some(now);
                },
                Action::Resume if !cancelled => {
                    if let Some(paused_at) = model.paused_at.take() {
                        let pause_duration = now.saturating_sub(paused_at);
                        model.last_time = model.last_time.saturating_add(pause_duration);
                    }
                },
                Action::Withdraw if !cancelled => {
                    let amount = model.claimable(now);
                    model.withdrawn = model.withdrawn.saturating_add(amount);
                    model.last_time = now;
                    paused_claimable = None;
                },
                Action::TopUp(amount) if !cancelled => {
                    let accrued = model.claimable(now);
                    model.deposited = model.deposited.saturating_add(amount as i128);
                    if model.paused_at.is_none() {
                        model.last_time = now;
                    }
                    let _ = accrued;
                },
                Action::Cancel if !cancelled => {
                    model.withdrawn = model.withdrawn.saturating_add(model.claimable(now));
                    cancelled = true;
                },
                _ => {}
            }

            prop_assert!(model.withdrawn >= previous_withdrawn);
            prop_assert!(model.withdrawn <= model.deposited);
            prop_assert!(model.deposited >= model.withdrawn.saturating_add(model.claimable(now)));
            if model.paused_at.is_some() && paused_claimable.is_some() && !cancelled {
                prop_assert!(model.claimable(now) - paused_claimable.unwrap() <= (model.deposited - deposited));
            }
            previous_withdrawn = model.withdrawn;
        }
    }
}

// ─── Issue #33 — Multi-pause property tests ───────────────────────────────────
//
// Property: across any sequence of pause/resume cycles the total accrual
// computed by the model is exactly `rate * active_seconds`, where
// `active_seconds` excludes all paused intervals. This is the core
// composability invariant for pause/resume time accounting.

/// A lightweight multi-pause model that explicitly tracks cumulative paused
/// seconds so the invariant can be stated as:
///   claimable(now) == rate * (now - start - total_paused_seconds) up to cap
#[derive(Clone, Debug)]
struct MultiPauseModel {
    deposited: i128,
    withdrawn: i128,
    rate: i128,
    start_time: u64,
    total_paused_seconds: u64,
    paused_at: Option<u64>,
}

impl MultiPauseModel {
    fn new(deposited: i128, rate: i128) -> Self {
        Self {
            deposited,
            withdrawn: 0,
            rate,
            start_time: 0,
            total_paused_seconds: 0,
            paused_at: None,
        }
    }

    /// Active seconds elapsed up to `now` (excluding all paused intervals).
    fn active_seconds(&self, now: u64) -> u64 {
        let wall = now.saturating_sub(self.start_time);
        let paused = if let Some(paused_at) = self.paused_at {
            self.total_paused_seconds
                .saturating_add(now.saturating_sub(paused_at))
        } else {
            self.total_paused_seconds
        };
        wall.saturating_sub(paused)
    }

    /// Claimable according to the explicit active-seconds formula.
    fn claimable(&self, now: u64) -> i128 {
        let active = self.active_seconds(now) as i128;
        let earned = active.saturating_mul(self.rate);
        let remaining = self.deposited.saturating_sub(self.withdrawn).max(0);
        earned.min(remaining)
    }
}

proptest! {
    #![proptest_config(ProptestConfig { cases: 5_000, .. ProptestConfig::default() })]
    #[test]
    fn multi_pause_resume_never_double_counts_time(
        deposited in 100i128..1_000_000_000i128,
        rate     in 1i128..10_000i128,
        // (active_secs_before_pause, pause_duration, active_secs_after_resume)
        triples in prop::collection::vec(
            (1u32..1_000u32, 1u32..1_000u32, 1u32..1_000u32),
            1..15,
        ),
    ) {
        let mut model = MultiPauseModel::new(deposited, rate);
        let mut now = 0u64;
        let mut total_active_seconds: u64 = 0;

        for (pre, pause, post) in triples {
            // Pre-pause active interval.
            now = now.saturating_add(pre as u64);
            total_active_seconds = total_active_seconds.saturating_add(pre as u64);

            // Start pause — model must not be already paused.
            if model.paused_at.is_some() {
                continue;
            }
            model.paused_at = Some(now);

            // Pause interval (no accrual).
            now = now.saturating_add(pause as u64);

            // Resume.
            if let Some(paused_at) = model.paused_at.take() {
                let pause_duration = now.saturating_sub(paused_at);
                model.total_paused_seconds = model.total_paused_seconds
                    .saturating_add(pause_duration);
            }

            // Post-resume active interval.
            now = now.saturating_add(post as u64);
            total_active_seconds = total_active_seconds.saturating_add(post as u64);

            // Invariant: active_seconds reported by model == our independent counter.
            prop_assert_eq!(
                model.active_seconds(now),
                total_active_seconds,
                "active_seconds must match independent tally: model={} tally={}",
                model.active_seconds(now),
                total_active_seconds
            );

            // Invariant: claimable == rate * active_seconds (capped at remaining).
            let remaining = model.deposited.saturating_sub(model.withdrawn).max(0);
            let expected_claimable = (total_active_seconds as i128)
                .saturating_mul(model.rate)
                .min(remaining);
            prop_assert_eq!(
                model.claimable(now),
                expected_claimable,
                "claimable must equal rate * active_seconds"
            );
        }
    }
}

proptest! {
    #![proptest_config(ProptestConfig { cases: 5_000, .. ProptestConfig::default() })]
    #[test]
    fn multi_pause_paused_seconds_never_accrue(
        deposited in 100i128..1_000_000_000i128,
        rate     in 1i128..10_000i128,
        active_secs in 1u32..10_000u32,
        pause_secs  in 1u32..10_000u32,
    ) {
        // Scenario: run active_secs, pause, run pause_secs, query claimable.
        // Claimable must equal rate * active_secs (capped at deposited).
        let mut model = MultiPauseModel::new(deposited, rate);
        let mut now = 0u64;

        // Active phase.
        now = now.saturating_add(active_secs as u64);
        let claimable_before_pause = model.claimable(now);

        // Pause.
        model.paused_at = Some(now);

        // Advance while paused.
        now = now.saturating_add(pause_secs as u64);

        // Claimable must not have grown during pause.
        let claimable_while_paused = model.claimable(now);
        prop_assert_eq!(
            claimable_while_paused,
            claimable_before_pause,
            "claimable must not grow while paused: before={} during={}",
            claimable_before_pause,
            claimable_while_paused
        );

        // Resume.
        if let Some(paused_at) = model.paused_at.take() {
            let pause_duration = now.saturating_sub(paused_at);
            model.total_paused_seconds = model.total_paused_seconds
                .saturating_add(pause_duration);
        }

        // Claimable right after resume == claimable right before pause.
        let claimable_after_resume = model.claimable(now);
        prop_assert_eq!(
            claimable_after_resume,
            claimable_before_pause,
            "claimable right after resume must equal claimable before pause"
        );

        // Core invariant: claimable == rate * active_seconds (capped).
        let remaining = deposited.max(0);
        let expected = (active_secs as i128)
            .saturating_mul(rate)
            .min(remaining);
        prop_assert_eq!(
            claimable_after_resume,
            expected,
            "claimable must equal rate * active_secs exactly"
        );
    }
}
