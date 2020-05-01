import tactic
import tactic.find
import tactic.core

-- Includes structural definitions and assumptions
namespace jitsynth

inductive aparam : Type
| reg : aparam
| bv : ℕ → aparam

inductive cparam : Type
| reg : ℕ → cparam
| bv : ℕ → ℕ → cparam

structure ainstr : Type :=
mk :: (op : ℕ) (F : list aparam)

structure cinstr : Type :=
mk :: (op : ℕ) (F : list cparam)

def cparam_in_aparam : aparam → cparam → Prop
| aparam.reg (cparam.reg n) := true
| aparam.reg (cparam.bv b c) := false
| (aparam.bv a) (cparam.reg n) := false
| (aparam.bv a) (cparam.bv b c) := a = b

def cinstr_in_ainstr : ainstr → cinstr → Prop
| (ainstr.mk n []) (cinstr.mk m []) := n = m
| (ainstr.mk n []) (cinstr.mk m l) :=
  n = m ∧ l = []
| (ainstr.mk n l) (cinstr.mk m []) :=
  n = m ∧ l = []
| (ainstr.mk n (ha::ta)) (cinstr.mk m (hc::tc)) :=
  (cparam_in_aparam ha hc) ∧
  (cinstr_in_ainstr
    (ainstr.mk n ta)
    (cinstr.mk m tc))

def cprog_in_aprog : list ainstr → list cinstr → Prop
| [] [] := true
| [] l := l = []
| l [] := l = []
| (ha::ta) (hc::tc) :=
  (cinstr_in_ainstr ha hc) ∧ (cprog_in_aprog ta tc)

def empty_cinstr : cinstr := (cinstr.mk 0 [])

def P : ainstr → list ℕ → option cinstr
| (ainstr.mk op list.nil) list.nil :=
  option.some (cinstr.mk op list.nil)
| (ainstr.mk op list.nil) (list.cons h t)  := option.none
| (ainstr.mk op (list.cons h t)) list.nil := option.none
| (ainstr.mk op (list.cons aparam.reg l)) (list.cons h t)
   := let pres := P (ainstr.mk op l) t in
        match pres with
        | option.none := option.none
        | option.some p :=
          option.some (cinstr.mk op (list.cons (cparam.reg h) (p.F)))
        end
| (ainstr.mk op (list.cons (aparam.bv n) l)) (list.cons h t)
   := let pres := P (ainstr.mk op l) t in
        match pres with
        | option.none := option.none
        | option.some p :=
          option.some (cinstr.mk op (list.cons (cparam.bv n h) (p.F)))
        end

structure state : Type :=
mk :: (regs : list ℕ) (mem : list ℕ) (pc : ℕ)

structure arm : Type :=
mk :: (T : cinstr → state → state) (Φ : list cinstr → ℕ)

-- ASSUMPTION: A congruence function exists,
constant reg_cong : list ℕ → list ℕ → Prop
constant mem_cong : list ℕ → list ℕ → Prop

inductive imp_param : Type
| sparam_sreg : ℕ → imp_param
| sparam_smem : ℕ → imp_param
| sparam_imm : ℕ → imp_param
| treg : ℕ → imp_param
| imm : ℕ → ℕ → imp_param
-- Exclusively produces an imm
| exp : (ℕ→ℕ) → imp_param → imp_param

structure imp_instr : Type :=
mk :: (op : ℕ) (F : list imp_param)

-- Takes a reg index mapping, a mem index mapping,
--   a concrete source instruction parameter list,
--   a target implementation instruction parameter list,
--   and outputs a concrete target parameter list
def concretize_param : (ℕ→ℕ) → (ℕ→ℕ) →
                       list cparam →
                       imp_param →
                       option cparam
| reg_map mem_map [] ip := none
| reg_map mem_map clist (imp_param.sparam_sreg n) :=
  match (list.nth clist n) with
  | option.none := none
  | option.some (cparam.bv a b) := none
  | option.some (cparam.reg r) := some (cparam.reg (reg_map r))
  end
| reg_map mem_map clist (imp_param.sparam_smem n) :=
  match (list.nth clist n) with
  | option.none := none
  | option.some (cparam.bv a b) := some (cparam.bv a (mem_map b))
  | option.some (cparam.reg r) := none
  end
| reg_map mem_map clist (imp_param.sparam_imm n) :=
  match (list.nth clist n) with
  | option.none := none
  | option.some (cparam.bv a b) := some (cparam.bv a b)
  | option.some (cparam.reg r) := none
  end
| reg_map mem_map clist (imp_param.treg n) := some (cparam.reg n)
| reg_map mem_map clist (imp_param.imm l n) := (cparam.bv l n)
| reg_map mem_map clist (imp_param.exp m ip) :=
  match (concretize_param reg_map mem_map clist ip) with
  | none := none
  | some (cparam.reg r) := none
  | some (cparam.bv n v) :=
    some (cparam.bv n (m v))
  end

def concretize_params : (ℕ→ℕ) → (ℕ→ℕ) →
                       list cparam →
                       list imp_param →
                       list (option cparam)
| rm mm clist [] := []
| rm mm clist (hi::ti) :=
  (concretize_param rm mm clist hi) ::
  (concretize_params rm mm clist ti)

def remove_option_helper {α : Type} :
  list (option α) →
  list α →
  option (list α)
| [] acc := some acc
| (h::t) acc :=
  match h with
  | none := none
  | some x := remove_option_helper t (x::acc)
  end

def remove_option {α : Type} :
  list (option α) → option (list α)
| l := remove_option_helper l []

def concretize_instr : (ℕ→ℕ) → (ℕ→ℕ) →
                       cinstr → imp_instr →
                       option cinstr
| reg_map mem_map (cinstr.mk cop cl) (imp_instr.mk iop il) :=
  match (remove_option
          (concretize_params
            reg_map mem_map cl il)) with
  | none := none
  | some params := some (cinstr.mk iop params)
  end

-- TODO should I use option?
def concretize_prog : (ℕ→ℕ) → (ℕ→ℕ) →
                      cinstr → list imp_instr →
                      list (option cinstr)
| reg_map mem_map ci [] := []
| reg_map mem_map ci (ii::il) :=
  (concretize_instr reg_map mem_map ci ii) ::
  (concretize_prog reg_map mem_map ci il)

-- ASSUMPTION: PC congruence is defined by pc_mult
constant pc_mult : ℕ 
-- source PC -> target PC -> bool
def pc_cong (spc : ℕ) (tpc : ℕ) : Prop :=
  pc_mult * spc = tpc
-- PC map is implicitly pc_mult * source PC

-- ASSUMPTION: no backwards jumps
constant no_back_jumps :
  ∀ m : arm, ∀ i : cinstr, ∀ σ : state,
  σ.pc < (m.T i σ).pc

def cong (σ₁ : state) (σ₂ : state) : Prop :=
  (reg_cong σ₁.regs σ₂.regs) ∧
  (mem_cong σ₁.mem σ₂.mem) ∧
  (pc_cong σ₁.pc σ₂.pc)

def run : list cinstr → state → (cinstr → state → state) → ℕ → state
| p σ T nat.zero := σ
| p σ T (nat.succ n) :=
  match (list.nth p σ.pc) with
  | option.none := σ
  | option.some pi := run p (T pi σ) T n
  end

-- def implementation : (sai : ainstr) 

def correct_impl (reg_map mem_map : ℕ→ℕ)
                 (s t : arm)
                 (sai : ainstr) (tap : list imp_instr) : Prop :=
  ∀ tp : list cinstr,
  ∀ sci : cinstr,
  ∀ tcp : list cinstr,
  ∀ σs σt : state,
  ∀ f : ℕ,
  (∃ conc_params : list ℕ,
    P sai conc_params = some sci) →
  remove_option
    (concretize_prog reg_map mem_map sci tap) =
    some tcp →
  cong σs σt →
  tp.length = σt.pc →
  pc_mult ≤ f →
  cong (s.T sci σs) (run (tp ++ tcp) σt t.T f)

def 𝔸 (mach : arm) (prog : list cinstr) (σ : state) : state :=
  run prog σ mach.T (mach.Φ prog)

def sound_minic
    (s t : arm) (C : cinstr → list cinstr) : Prop :=
  ∀ tp : list cinstr,
  ∀ si : cinstr,
  ∀ σs σt : state,
  ∀ f : ℕ,
  (cong σs σt) →
  tp.length = σt.pc →
  pc_mult ≤ f →
  (cong (s.T si σs)
        (run (tp ++ (C si)) σt (t.T) f))

def sound_minic_instr
    (s t : arm) (C : cinstr → list cinstr) (si : cinstr): Prop :=
  ∀ tp : list cinstr,
  ∀ σs σt : state,
  ∀ f : ℕ,
  (cong σs σt) →
  tp.length = σt.pc →
  pc_mult ≤ f →
  (cong (s.T si σs)
        (run (tp ++ (C si)) σt (t.T) f))

def compile : (cinstr → list cinstr) → list cinstr → list cinstr
| C list.nil := list.nil
| C (list.cons si t) := (C si) ++ (compile C t)

end jitsynth