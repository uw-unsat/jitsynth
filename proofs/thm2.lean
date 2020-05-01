import tactic
import tactic.find
import tactic.core

import .lib
import .util.nat
import .util.list

namespace jitsynth

def Naive_param_0 : imp_param → Prop
| param :=
  (∃ n m,
     param = imp_param.sparam_sreg n ∨
     param = imp_param.sparam_smem n ∨
     param = imp_param.sparam_imm n ∨
     param = imp_param.treg n ∨
     param = imp_param.imm n m)

def Naive_param : ℕ → imp_param → Prop
| 0 param := Naive_param_0 param
| (d+1) param :=
  Naive_param_0 param ∨
  ∃ subparam f,
    Naive_param d subparam ∧
    param = imp_param.exp f subparam

def Naive_instr : ℕ → imp_instr → Prop
| d (imp_instr.mk op []) := true
| d (imp_instr.mk op (h::t)) :=
  Naive_param d h ∧
  Naive_instr d (imp_instr.mk op t)

def Naive_helper : ℕ → list imp_instr → Prop
| d [] := true
| d (h::t) := Naive_instr d h ∧ Naive_helper d t

def Naive : ℕ → ℕ → list imp_instr → Prop
| k d imp := imp.length ≤ k ∧ Naive_helper d imp

-- TODO derive enumeration somehow
def enumerate_Naive : ℕ → ℕ → ℕ → ℕ
| k d op := k * d * op

/-
-- Same proof for any r/w set mapping
constant rw_param_match : aparam → imp_param → Prop
constant rw_instr_match : ℕ → ℕ → Prop

def RW_param_0 : aparam → imp_param → Prop
| ap ip :=
  rw_param_match ap ip ∧
  (∃ n m,
     ip = imp_param.sparam_sreg n ∨
     ip = imp_param.sparam_smem n ∨
     ip = imp_param.sparam_imm n ∨
     ip = imp_param.treg n ∨
     ip = imp_param.imm n m)

def RW_param : ℕ → aparam → imp_param → Prop
| 0 ap ip := RW_param_0 ap ip
| (d+1) ap ip :=
  RW_param_0 ap ip ∨
  ∃ subparam f,
    RW_param d ap ip ∧
    ip = imp_param.exp f subparam

def RW_params : ℕ → list (aparam × imp_param) → Prop
| d [] := true
| d [(ap, ip) :: tl] :=

def RW_instr : ℕ → ainstr → imp_instr → Prop
| d (ainstr.mk aop []) (imp_instr.mk iop []) :=
| d ai (imp_instr.mk op (h::t)) :=
  RW_param d h ∧
  RW_instr d (imp_instr.mk op t)

def RW_helper : ℕ → list imp_instr → Prop
| d [] := true
| d (h::t) := RW_instr d h ∧ RW_helper d t

def RW : ℕ → ℕ → list imp_instr → Prop
| k d imp := imp.length ≤ k ∧ RW_helper d imp
-- TODO parameter loads
-/


-- For completeness, want to say that
--  sketches in RW, PLD, and Naive are finite for given k and d
theorem completeness :
  ∀ k d ops : ℕ, ∃ x : ℕ, x = enumerate_Naive k d ops
| k d ops :=
begin
  apply exists.intro,
  reflexivity
end

-- For soundness, want to say that
--  sketches in RW, PLD, and Naive obey sound_minic
--  when they are correct_impl
theorem soundness :
  ∀ k d : ℕ,
  ∀ imp : list imp_instr,
  ∀ regmap memmap : (ℕ→ℕ),
  ∀ s t : arm,
  ∀ sai : ainstr,
  ∀ sci : cinstr,
  ∀ C : (cinstr → list cinstr),
  (∃ params : list ℕ, P sai params = some sci) →
  (∀ ci : cinstr,
    remove_option
      (concretize_prog regmap memmap ci imp) =
      some (C ci)) →
  Naive k d imp →
  correct_impl regmap memmap s t sai imp →
  sound_minic_instr s t C sci
| k d imp regmap memmap s t sai sci C eparams Ch nh cih :=
begin
  unfold sound_minic_instr,
  intros tp σs σt f congh tpch fbound,
  unfold correct_impl at cih,
  apply cih,
  ---
  apply eparams,
  ---
  apply Ch,
  ---
  repeat {assumption}
end

end jitsynth