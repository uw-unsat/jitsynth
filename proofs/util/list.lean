import tactic
import tactic.find
import tactic.core

import .nat

-- Lemmas involving facts about lists
namespace jitsynth

def sublist {α : Type} : list α → ℕ → list α
| [] n := []
| l 0 := []
| (a :: b) (n+1) := a :: (sublist b n)

lemma nth_big_i :
  ∀ α : Type,
  ∀ l : list α,
  ∀ i : ℕ,
  l.length ≤ i → list.nth l i = option.none
| α list.nil i :=
  by intros _; unfold list.nth
| α (list.cons h l) 0 := by simp
| α (list.cons h l) (nat.succ i) :=
  begin
    simp; rewrite nat.one_add,
    intros sh,
    have h : l.length ≤ i,
      apply nat.le_of_succ_le_succ, exact sh,
    apply nth_big_i, exact h
  end

lemma sublist_length_0 {α : Type}:
  ∀ l : list α,
  (sublist l 0).length = 0
| [] := by unfold sublist; simp
| (a :: b) := by unfold sublist; simp

lemma sublist_length {α : Type}:
  ∀ l : list α,
  ∀ n : ℕ,
  n ≤ l.length →
  (sublist l n).length = n
| [] n b :=
begin
  simp at b,
  -- Automated
  -- rewrite nat.eq_zero_of_le_zero b,
  rewrite b,
  unfold sublist,
  simp,
end
| l 0 b :=
by apply sublist_length_0
| (h :: t) (n+1) b :=
begin
  unfold sublist,
  simp,
  rewrite nat.add_comm,
  have h₀ := sublist_length t n,
  simp at b,
  rewrite nat.add_comm at b,
  have h₁ := nat.le_of_add_le_add_left b,
  have h₂ := (sublist_length t n) h₁,
  rewrite h₂,
end

lemma e_nth (α : Type) :
  ∀ l : list α,
  ∀ n : ℕ,
  ∀ a : α,
  n < l.length →
  ∃ p, some p = list.nth l n
| [] n a h := by simp at *; cases h
| (x::xs) 0 a h := by simp at *
| (x::xs) (n+1) a h :=
begin
  simp at *,
  rewrite nat.add_comm at h,
  have h' := nat.lt_of_add_lt_add_left h,
  apply e_nth,
  repeat {assumption}
end

lemma nth_sub {α : Type}:
  ∀ hp tp : list α,
  ∀ p : α,
  ∀ n : ℕ,
  hp.nth n = some p →
  (hp++tp).nth n = some p
| [] tp p n p_def := by cases p_def
| (hh::hp) tp p 0 p_def := by simp at *; assumption
| (hh::hp) tp p (n+1) p_def :=
begin
  simp at *,
  apply nth_sub,
  apply p_def
end

lemma append_length {α : Type}:
  ∀ a b : list α,
  a.length + b.length = (a++b).length
| a b := by simp

end jitsynth