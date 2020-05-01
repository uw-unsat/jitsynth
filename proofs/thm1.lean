import tactic
import tactic.find
import tactic.core

import .lib
import .util.nat
import .util.list

namespace jitsynth

-- If starting PC is too big,
-- Result of run is the starting state
lemma run_big_pc :
  ∀ m : arm,
  ∀ p : list cinstr,
  ∀ σ : state,
  ∀ T : cinstr → state → state,
  ∀ φ : ℕ,
  p.length ≤ σ.pc →
  (run p σ T φ) = σ 
| m p σ T 0 :=
  by intro h; unfold run
| m p σ T (nat.succ φ) :=
  begin
    intro h,
    unfold run,
    rewrite nth_big_i; try {exact h},
    unfold run._match_1
  end

-- Simplify the length of a compiled program
lemma compile_len :
  ∀ sp : list cinstr,
  ∀ C : cinstr → list cinstr,
  (∀ si : cinstr, (C si).length = pc_mult) →
  (compile C sp).length = pc_mult * sp.length
| [] C h := by unfold compile;simp
| (sph :: spt) C h :=
begin
  unfold compile,
  simp,
  rewrite h,
  rewrite nat.left_distrib,
  simp,
  rewrite compile_len,
  exact h
end

-- Can reduce/increase the amount of fuel
--   if the smaller fuel value is enough
--   to complete the run
lemma enough_fuel :
  ∀ m : arm,
  ∀ p : list cinstr,
  ∀ σ : state,
  ∀ f big_f : ℕ,
  p.length - σ.pc ≤ f →
  f ≤ big_f →
  run p σ m.T f = run p σ m.T big_f
| m p σ 0 big_f f_bound big_f_bound :=
begin
  -- nat.le_of_sub_eq_zero
  have f_bound' := nat.eq_zero_of_le_zero f_bound,
  have pc_bound := nat.le_of_sub_eq_zero f_bound',
  repeat {rewrite run_big_pc},
  repeat {assumption}
end
| m p σ (f+1) big_f f_bound big_f_bound :=
begin
  cases nat.lt_or_ge σ.pc p.length,
    -- σ.pc < p.length
    have e_p_nth := e_nth cinstr p σ.pc empty_cinstr h,
    cases e_p_nth with p_pc some_p_pc,
    unfold run,
    rewrite ← some_p_pc,
    unfold run._match_1,
    rewrite (enough_fuel m p (m.T p_pc σ) f (big_f - 1)),
    have one_le_big_f : 1 ≤ big_f,
      apply one_le_of_add_le,
      rewrite nat.add_comm,
      apply big_f_bound,
    have big_f_decomp := sub_succ big_f one_le_big_f,
    have rewrite_right :
      run p σ m.T big_f = run p σ m.T (big_f - 1 + 1),
      rewrite big_f_decomp,
    rewrite rewrite_right,
    unfold run,
    rewrite ← some_p_pc,
    unfold run._match_1,
    -- p.len - newpc ≤ f
    apply sub_le_of_sub_le_succ_of_lt,
    apply no_back_jumps,
    assumption,
    -- f ≤ big_f - 1
    apply succ_le_le_sub,
    assumption,
    -- σ.pc ≥ p.length
    repeat {rewrite run_big_pc},
    repeat {assumption}
end

-- Progress "run" by one step
lemma head_run_somepc :
  ∀ m : arm,
  ∀ p : list cinstr,
  ∀ pi : cinstr,
  ∀ σ : state,
  ∀ f : ℕ,
  some pi = p.nth σ.pc →
  1 ≤ f →
  run p σ m.T f = 
  run p (m.T pi σ) m.T (f - 1)
| m p pi σ 0 pc0 f_bound := by cases f_bound
| m p pi σ (f+1) pc0 f_bound :=
begin
  simp,
  unfold run,
  rewrite ← pc0,
  unfold run._match_1
end

-- Can run the head of a program and reduce fuel
--   when enough fuel exists
lemma head_run :
  ∀ m : arm,
  ∀ hp : cinstr,
  ∀ tp : list cinstr,
  ∀ σ : state,
  ∀ f : ℕ,
  1 + tp.length ≤ f →
  run (hp::tp) σ m.T f = 
  run (hp::tp) (run [hp] σ m.T 1) m.T (f - 1)
| m hp tp σ 0 f_bound :=
begin
  exfalso,
  apply nat.not_succ_le_zero,
  rewrite ← nat.add_one,
  rewrite nat.add_comm,
  assumption
end
| m hp tp σ 1 f_bound :=
begin
  simp at *,
  unfold run,
  have tp_nil := list.eq_nil_of_length_eq_zero f_bound,
  rewrite tp_nil
end
| m hp tp σ (f+1) f_bound :=
begin
  simp,
  cases nat.lt_or_ge 0 σ.pc with pc_bound pc_bound,
  -- σ.pc > 0
  have inner_simp : run [hp] σ m.T 1 = σ,
    apply run_big_pc; assumption; simp,
  rewrite inner_simp,
  rewrite ← enough_fuel,
  -- 1 + tp.length - σ.pc ≤ f
  simp,
  apply sub_le_of_sub_le_succ_of_lt,
    assumption,
    simp; assumption,
  --
  apply nat.le_succ,
  -- σ.pc = 0
  have pc_zero : σ.pc = 0,  
    apply nat.eq_zero_of_le_zero; assumption,
  unfold run,
  rewrite pc_zero,
  simp,
  unfold run._match_1
end

-- If a program length is bound by fuel and starting PC,
--   then it's also bound by the resulting PC
lemma run_length_bound_pc_helper :
  ∀ m : arm,
  ∀ p : list cinstr,
  ∀ σ : state,
  ∀ f : ℕ,
  p.length ≤ f + σ.pc →
  p.length ≤ (run p σ m.T f).pc
| m p σ 0 bound :=
begin
  unfold run,
  simp at bound,
  apply bound
end
| m p σ (f+1) bound :=
begin
  cases nat.lt_or_ge σ.pc p.length,
    have e_pi_nth := e_nth cinstr p σ.pc empty_cinstr h,
    cases e_pi_nth with pi pi_nth,
    unfold run,
    rewrite ← pi_nth,
    unfold run._match_1,
    apply run_length_bound_pc_helper,
    apply nat.le_trans,
    apply bound,
    rewrite nat.add_assoc,
    apply nat.add_le_add_left,
    apply nat.le_of_lt_succ,
    rewrite ← nat.add_one,
    rewrite nat.add_comm,
    apply nat.add_lt_add_right,
    apply no_back_jumps,
    ---
    rewrite run_big_pc,
    repeat {assumption}
end

-- If a program length is bound by fuel,
--   then it's also bound by the resulting PC
lemma run_length_bound_pc :
  ∀ m : arm,
  ∀ p : list cinstr,
  ∀ σ : state,
  ∀ f : ℕ,
  p.length ≤ f →
  p.length ≤ (run p σ m.T f).pc
| m p σ f bound :=
begin
  rewrite ← enough_fuel m p σ p.length f,
  apply run_length_bound_pc_helper,
  apply nat.le_add_right,
  apply sub_le,
  assumption
end

-- Can run the first part of a program with enough fuel,
--   proven with induction over the difference between
--   the length of this smaller program and the starting PC
lemma head_prog_run_subpc_pciter :
  ∀ m : arm,
  ∀ hp tp : list cinstr,
  ∀ σ : state,
  ∀ k f : ℕ,
  hp.length - σ.pc = k →
  hp.length + tp.length ≤ f + σ.pc →
  run (hp++tp) σ m.T f = 
  run (hp++tp) (run hp σ m.T (hp.length - σ.pc)) m.T (f - (hp.length - σ.pc))
| m [] tp σ pc f k_def f_bound := by simp; unfold run
| m (hh::hp) tp σ 0 f k_def f_bound :=
begin
  have pc_ineq := nat.le_of_sub_eq_zero k_def,
  have rw_inner : (run (hh::hp) σ m.T ((hh::hp).length - σ.pc)) = σ,
    apply run_big_pc m,
    apply pc_ineq,
  rewrite rw_inner,
  rewrite k_def,
  simp
end
| m hp tp σ k 0 k_def f_bound :=
begin
  simp at *,
  unfold run,
  rewrite run_big_pc m,
  apply nat.le_trans,
  apply nat.le_add_right,
  exact tp.length,
  assumption
end
| m (hh::hp) tp σ (k+1) (f+1) k_def f_bound :=
begin
  -- Try to increase the fuel on the left and right side
  -- So that I can use the head_run on the left
  have rw_inner :
    run (hh::hp) σ m.T ((hh::hp).length - σ.pc) =
    run (hh::hp) σ m.T (hh::hp).length,
    rewrite enough_fuel,
    reflexivity,
    apply sub_le,
  rewrite rw_inner,
  have pc_ineq : σ.pc < (hh::hp).length,
    apply lt_sub_succ_n; assumption,
  have e_p_nth := e_nth cinstr (hh::hp) σ.pc empty_cinstr pc_ineq,
  cases e_p_nth with p p_nth,
  have nth_hptp : (hh::hp++tp).nth σ.pc = some p,
    apply nth_sub,
    rewrite p_nth,
  ----
  unfold run,
  rewrite nth_hptp,
  unfold run._match_1,
  ----
  -- do head_run_somepc on the inner one
  have rw_inner2 :
    run (hh::hp) σ m.T (hh::hp).length =
    run (hh::hp) (m.T p σ) m.T ((hh::hp).length - 1),
    rewrite head_run_somepc,
    assumption,
    simp; apply nat.le_add_right,
  rewrite rw_inner2,
  have eval_ge_1 : 1 ≤ (m.T p σ).pc,
      apply nat.le_of_lt_succ,
      rewrite ← nat.add_one,
      simp,
      apply nat.lt_of_le_of_lt,
      apply nat.zero_le,
      apply no_back_jumps,
  ----
  rewrite head_prog_run_subpc_pciter,
    ---
    have rw_inner2 :
      run (hh::hp) (m.T p σ) m.T ((hh::hp).length - (m.T p σ).pc) =
      run (hh::hp) (m.T p σ) m.T ((hh::hp).length - 1),
      apply enough_fuel,
      reflexivity,
      apply nat.sub_le_sub_left,
      apply eval_ge_1,
    rewrite rw_inner2,
    have my_inner_pc_bound :
      (hh::hp).length ≤ (run (hh::hp) (m.T p σ) m.T ((hh::hp).length - 1)).pc,
      apply run_length_bound_pc_helper,
      simp,
      rewrite nat.add_comm,
      apply nat.add_le_add_left,
      apply eval_ge_1,
    -- Need to prove both of these inequalities
    ---- First
    have trans_lemma1 :
        (hh::hp++tp).length - (run (hh::hp) (m.T p σ) m.T ((hh::hp).length - 1)).pc ≤
        (hh::hp++tp).length - (hh::hp).length,
      apply nat.sub_le_sub_left,
      apply my_inner_pc_bound,
    have trans_lemma2 :
        tp.length ≤ f - (hp.length - σ.pc),
      simp at f_bound,
      rewrite nat.add_comm at f_bound,
      rewrite ← nat.add_assoc at f_bound,
      have f_bound' := nat.le_of_add_le_add_right f_bound,
      rewrite nat.add_comm at f_bound',
      have f_bound'' := nat.sub_le_sub_right f_bound' σ.pc,
      rewrite nat.add_sub_assoc at f_bound'',
      rewrite nat.add_sub_cancel at f_bound'',
      have f_bound''' := nat.sub_le_sub_right f_bound'' (hp.length - σ.pc),
      rewrite nat.add_sub_cancel at f_bound''',
      assumption,
      ---
      apply nat.le_of_lt_succ,
      simp at pc_ineq,
      rewrite nat.add_comm at pc_ineq,
      rewrite nat.add_one at pc_ineq,
      assumption,
    have len_add_rw : 1 + (hp.length + tp.length) = tp.length + (1 + hp.length),
      simp,
    have enough_fuel_ineq1 :
        (hh::hp++tp).length - (run (hh::hp) (m.T p σ) m.T ((hh::hp).length - 1)).pc ≤
        (f - ((hh::hp).length - (m.T p σ).pc)),
      apply nat.le_trans trans_lemma1,
      simp,
      rewrite len_add_rw,
      rewrite nat.add_sub_cancel,
      apply nat.le_trans trans_lemma2,
      apply nat.sub_le_sub_left,
      apply nat.le_of_lt_succ,
      rewrite ← nat.one_add,
      rewrite ← nat.add_sub_assoc,
      apply smaller_sub,
      apply no_back_jumps,
      simp at pc_ineq,
      assumption,
      apply nat.le_of_lt_succ,
      simp at pc_ineq,
      rewrite one_add,
      assumption,
    -------
    ---- Second
    have enough_fuel_ineq2 :
       (hh::hp++tp).length - (run (hh::hp) (m.T p σ) m.T ((hh::hp).length - 1)).pc ≤
       (f + 1 - ((hh::hp).length - σ.pc)),
      -- NOTE: This should be strictly easier than the last one,
      --       Since this was basically a sub-proof
      --       (can basically get + 1 and hh to cancel, going smaller)
      apply nat.le_trans trans_lemma1,
      simp,
      rewrite len_add_rw,
      rewrite nat.add_sub_cancel,
      apply nat.le_trans trans_lemma2,
      have rw_asa : 1 + hp.length - σ.pc = (hp.length - σ.pc) + 1,
        simp,
        apply nat.add_sub_assoc,
        apply nat.le_of_lt_succ,
        simp at pc_ineq,
        rewrite one_add,
        assumption,
      rewrite rw_asa,
      rewrite nat.add_sub_add_right,
    --------
    cases nat.lt_or_ge
          (f - ((hh::hp).length - (m.T p σ).pc))
          (f + 1 - ((hh::hp).length - σ.pc)),
      -- case 1
      apply enough_fuel,
      apply enough_fuel_ineq1,
      apply nat.le_of_lt; assumption,
      -- case 2
      apply symm,
      apply enough_fuel,
      apply enough_fuel_ineq2,
      assumption,
    ------
    reflexivity,
    ---
    apply nat.le_trans,
    assumption,
    rewrite nat.add_assoc,
    apply nat.add_le_add_left,
    apply nat.le_of_lt_succ,
    rewrite ← nat.add_one,
    rewrite nat.add_comm,
    apply nat.add_lt_add_right,
    apply no_back_jumps
    ---
end

-- Can run the first part of a program with enough fuel
lemma head_prog_run_subpc :
  ∀ m : arm,
  ∀ hp : list cinstr,
  ∀ tp : list cinstr,
  ∀ σ : state,
  ∀ f : ℕ,
  hp.length + tp.length ≤ f + σ.pc →
  run (hp++tp) σ m.T f = 
  run (hp++tp) (run hp σ m.T (hp.length - σ.pc)) m.T (f - (hp.length - σ.pc))
| m hp tp σ f f_bound :=
begin
  apply head_prog_run_subpc_pciter,
  reflexivity,
  assumption
end
---

-- For a non-empty source program,
--   can decompose the compiled program
--   into a compiled first sequence,
--   a compiled single source instruction,
--   and some tailing program
lemma decompose_compile :
  ∀ C : cinstr → list cinstr,
  ∀ sp : list cinstr,
  ∀ p : cinstr,
  ∀ pc : ℕ,
  sp.nth pc = some p →
  ∃ t : list cinstr,
  (compile C sp) = (compile C (sublist sp pc)) ++ (C p) ++ t
| C [] p pc smp := by cases smp
| C (a::sp) p pc smp :=
begin
  cases nat.lt_or_ge pc 1,
  apply exists.intro,
  unfold compile,
  -- pc < 1
  have pc0 := only_zero_lt_one pc h,
  rewrite pc0,
  unfold sublist,
  unfold compile,
  simp,
  have pa : p = a,
    rewrite pc0 at smp,
    simp at smp,
    rewrite smp,
  rewrite pa,
  -- pc ≥ 1
  have pcm1 : pc = (pc - 1) + 1,
    rewrite sub_succ,
    apply h,
  rewrite pcm1,
  unfold sublist,
  unfold compile,
  have smp_sub1 : sp.nth (pc-1) = some p,
    rewrite pcm1 at smp,
    unfold list.nth at smp,
    assumption,
  have ih := decompose_compile C sp p (pc-1) smp_sub1,
  cases ih with comp_tail rw_comp,
  rewrite rw_comp,
  simp,
  existsi comp_tail,
  reflexivity
end

-- Given necessary assumptions,
--   for any starting target state and fuel,
--   there exists a new target state and smaller fuel value
--   such that a "run" with the starting state and fuel
--   is equivalent to a run with this new state
--   and reduced fuel.
--   In addition, this new target state is congruent to
--   the source state one step after the starting source state,
--   and the reduced fuel is reduced by pc_mult at most.
lemma sm_to_e_cong_tstate :
  ∀ s t : arm,
  ∀ C : cinstr → list cinstr,
  (sound_minic s t C) →
  (∀ si : cinstr, (C si).length = pc_mult) →
    ∀ sp : list cinstr,
    ∀ p : cinstr,
    ∀ σs σt : state,
    ∀ t_k : ℕ,
      (cong σs σt) →
      σs.pc < sp.length →
      some p = sp.nth σs.pc →
      pc_mult * (sp.length - σs.pc) ≤ t_k →
      ∃ σt' t_k',
        (cong (s.T p σs) σt') ∧
        t_k' ≤ t_k ∧
        t_k - t_k' ≤ pc_mult ∧
        (run (compile C sp) σt (t.T) t_k) = 
          (run (compile C sp) σt' (t.T) t_k')
| s t C lsm csize sp p σs σt t_k cst pc_bound some_p tk_bound :=
begin
  unfold sound_minic at lsm,
  existsi (run ((compile C (sublist sp σs.pc)) ++ C p) σt (t.T) pc_mult),
  -- Previously tried, but not as nice
  -- existsi t_k - (used_fuel ((compile C (sublist sp σs.pc)) ++ C p) σt (t.T) pc_mult),
  existsi t_k - pc_mult,
  apply and.intro,
    apply lsm,
      apply cst,
      -----
      unfold cong at cst,
      cases cst with rcst cst,
      cases cst with mcst pcst,
      unfold pc_cong at pcst,
      rewrite compile_len,
      rewrite sublist_length,
      exact pcst,
      ----
      apply le_of_lt; apply pc_bound,
    apply csize,
    reflexivity,
  apply and.intro,
    -- t_k - used_fuel ≤ t_k
    apply sub_le,
  apply and.intro,
    rewrite nat.sub_sub_self,
    have sp_diff_ge_1 := diff_ge_1 σs.pc sp.length pc_bound,
    rewrite ← nat.mul_one pc_mult,
    have first_trans : pc_mult * 1 ≤ pc_mult * (sp.length - σs.pc),
      cases nat.lt_or_ge 0 pc_mult,
        -- 0 < pc_mult
        rewrite mul_le_mul_left,
        apply sp_diff_ge_1,
        assumption,
        -- pc_mult = 0
        simp at h,
        rewrite h,
        simp,
    apply nat.le_trans,
    assumption,
    assumption,
  -- head_prog_run
  -- decompose compile C sp
  have decomp :=
    decompose_compile
      C sp p σs.pc
      (eq.symm some_p),
  cases decomp with t_prog rw_comp,
  rewrite rw_comp,
   have pc_mult_rw :
    pc_mult = ((compile C (sublist sp σs.pc)) ++ C p).length - σt.pc,
    simp,
    rewrite csize,
    rewrite compile_len,
    rewrite sublist_length,
    unfold cong at cst,
    cases cst with rcst cst,
    cases cst with mcst pcst,
    unfold pc_cong at pcst,
    rewrite ← pcst; simp,
    apply nat.le_of_lt,
    exact pc_bound,
    exact csize,
  
  have tk_pc_diff_rw :
    t_k - pc_mult = t_k - (((compile C (sublist sp σs.pc)) ++ C p).length - σt.pc),
    rewrite pc_mult_rw,
  rewrite tk_pc_diff_rw,
  rewrite pc_mult_rw,
  apply head_prog_run_subpc t ((compile C (sublist sp σs.pc)) ++ C p) t_prog σt t_k,
  simp,
  rewrite nat.add_comm,
  have rw_left := nat.add_comm (C p).length (compile C (sublist sp σs.pc)).length,
  rewrite rw_left,
  rewrite append_length,
  rewrite append_length,
  rewrite ← rw_comp,
  rewrite compile_len,
  unfold cong at cst,
  cases cst with reg_cst cst,
  cases cst with mem_cst pc_cst,
  unfold pc_cong at pc_cst,
  rewrite ← pc_cst,
  rewrite ← nat.add_le_add_iff_le_right (pc_mult * σs.pc) at tk_bound,
  rewrite ← nat.left_distrib at tk_bound,
  rewrite nat.sub_add_cancel at tk_bound,
  assumption,
  apply nat.le_of_lt,
  repeat {assumption}
end

-- Prove the main theorem with fewer restrictions
--   to allow for a stronger inductive hypothesis
theorem sound_run_jit : 
  ∀ s t : arm, ∀ sp : list cinstr,
  ∀ C : cinstr → list cinstr,
  ∀ σs σt : state,
  ∀ k t_k : ℕ,
  sp.length - σs.pc ≤ k →
  pc_mult * k ≤ t_k →
  (∀ si : cinstr, (C si).length = pc_mult) →
  (sound_minic s t C) →
  (cong σs σt) →
  (cong (run sp σs s.T k)
        (run (compile C sp) σt t.T t_k))
| s t sp C σs σt nat.zero t_k :=
  begin
    intros kl0 tkg Cilen_eq_pcmul sml cst,
    have k0 : 0 = sp.length - σs.pc,
      symmetry,
      apply nat.eq_zero_of_le_zero; apply kl0,
    have len_leq_pc := eq.symm k0,
    rewrite nat.sub_eq_zero_iff_le at len_leq_pc,
    have tpc_pcmul_x_spc : σt.pc = pc_mult * σs.pc,
      unfold cong at cst; unfold pc_cong at cst,
      apply eq.symm,
      exact cst.right.right,
    have t_len_leq_pc : (list.length (compile C sp)) ≤ σt.pc,
      rewrite tpc_pcmul_x_spc,
      rewrite compile_len,
      apply nat.mul_le_mul_left,
      repeat {assumption},
    rewrite run_big_pc,
      rewrite run_big_pc,
      repeat {assumption}
  end
| s t sp C σs σt (nat.succ k) t_k :=
  begin
    intros kn tkg cilen sml cst,
    have pc_lt_or_ge := nat.lt_or_ge σs.pc sp.length,
    apply or.elim pc_lt_or_ge; intro pc_ineq, 
    -- case: σs.pc < list.length sp ----------------------
    have e_sp_nth : ∃ p, some p = list.nth sp σs.pc,
      apply e_nth,
      exact empty_cinstr,
      assumption,
    cases e_sp_nth with p sp_nth,
    have rewrite_left :
      (run sp σs (s.T) (nat.succ k)) =
      (run sp (s.T p σs) (s.T) k),
      unfold run,
      rewrite ← sp_nth,
      unfold run._match_1,
    rewrite rewrite_left,
    have e_cong_tstate :
      ∃ σt' t_k',
      (cong (s.T p σs) σt') ∧
      pc_mult * k ≤ t_k' ∧
      (run (compile C sp) σt (t.T) t_k) = 
      (run (compile C sp) σt' (t.T) t_k'),
      have t_k_bound : pc_mult * (sp.length - σs.pc) ≤ t_k,
        apply mul_le_inside,
        assumption,
        assumption,
      have e_cong_sub :=
        (sm_to_e_cong_tstate
          s t C sml cilen
          sp p σs σt t_k
          cst pc_ineq sp_nth t_k_bound),
      cases e_cong_sub with σt' e_cong_sub,
      cases e_cong_sub with t_k' e_cong_sub,
      cases e_cong_sub with cst' e_cong_sub,
      cases e_cong_sub with tk'_le_tk e_cong_sub,
      cases e_cong_sub with diff_bound e_cong_rewrite,
      existsi σt'; existsi t_k',
      apply and.intro, assumption,
      apply and.intro,
        -- pc_mult * k ≤ t_k'
        rewrite nat.mul_succ at tkg,
        have tkg' := nat.sub_le_sub_right tkg pc_mult,
        simp at tkg',
        apply nat.le_trans,
        apply tkg',
        -- rewrite nat.add_le_to_le_sub at diff_bound,
        have diff_bound' := nat.add_le_add_right diff_bound t_k',
        rewrite nat.sub_add_cancel at diff_bound',
        have diff_bound'' := nat.sub_le_sub_right diff_bound' pc_mult,
        simp at diff_bound'',
        assumption,
        assumption,
        assumption,
      -- 
    cases e_cong_tstate with σt' e_cong_tstate,
    cases e_cong_tstate with t_k' e_cong_tstate,
    cases e_cong_tstate with cong_state e_cong_tstate,
    cases e_cong_tstate with t_k'_bound rewrite_right,
    rewrite rewrite_right,
    apply sound_run_jit,
    -- goal: list.length sp - (s.T p σs).pc ≤ k
    have nbj := no_back_jumps s p σs,
    have ineq1 : list.length sp - (s.T p σs).pc ≤ list.length sp - σs.pc,
      apply nat.sub_le_sub_left,
      apply nat.le_of_lt nbj,
    have ineq2 : list.length sp - (s.T p σs).pc < list.length sp - σs.pc,
    apply sub_lt_sub,
    apply no_back_jumps,
    exact pc_ineq,
    apply nat.le_of_lt_succ,
    apply nat.lt_of_lt_of_le,
    exact ineq2,
    repeat {assumption},
    -- case: σs.pc < list.length sp ----------------------
    repeat {rewrite run_big_pc; try {assumption}},
    unfold cong at cst,
    cases cst with rcst cst; cases cst with mcst pcst,
    unfold pc_cong at pcst,
    rewrite ← pcst,
    rewrite compile_len,
    apply nat.mul_le_mul_left,
    repeat {assumption}
  end

-- Main theorem
-- States that for any pair of machines,
--   any source program, any compiler,
--   and any starting state pair,
--   given compiled programs are "pc_mult" long,
--   given the starting fuel is the program length,
--   given the individual compilers are sound,
--   given the starting states are congruent,
--   then the result of executing the source program
--   is congruent to the result of executing the
--   compiled source program
theorem jit_soundness : 
  ∀ s t : arm, ∀ sp : list cinstr,
  ∀ C : cinstr → list cinstr,
  ∀ σs σt : state,
  (∀ si : cinstr, (C si).length = pc_mult) →
  (∀ p : list cinstr, s.Φ p = p.length) →
  (∀ p : list cinstr, t.Φ p = p.length) →
  (sound_minic s t C) →
  (cong σs σt) →
  (cong (𝔸 s sp σs) (𝔸 t (compile C sp) σt))
| s t sp C σs σt clen sfuel_bound tfuel_bound sml cst :=
begin
  unfold 𝔸,
  rewrite sfuel_bound,
  rewrite tfuel_bound,
  apply sound_run_jit,
  apply sub_le,
  rewrite compile_len,
  repeat {assumption}
end

end jitsynth