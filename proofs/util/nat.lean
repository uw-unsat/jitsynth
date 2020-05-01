import tactic
import tactic.find
import tactic.core

-- Lemmas involving facts about ℕ
namespace jitsynth

lemma sub_lt_sub : 
      ∀ a b c : ℕ,
      c < b →
      c < a →
      a - b < a - c
| 0 b c h₁ h₂ :=
-- base case
begin
  have c_le_zero := nat.le_of_lt h₂,
  have c_eq_zero := nat.eq_zero_of_le_zero c_le_zero,
  rewrite c_eq_zero at h₂,
  repeat {rewrite nat.zero_sub},
  exact h₂
end
| (a+1) b c h₁ h₂ :=
-- inductive case
begin
  have ih := sub_lt_sub a b c,
  have h₃ := nat.succ_le_of_lt h₂,
  have hc : nat.succ c = c + 1,
    simp,
  rewrite hc at h₃,
  have h₄ : c ≤ a,
    rewrite ← nat.add_le_add_iff_le_right,
    apply h₃,
  have h₅ := nat.eq_or_lt_of_le h₄,
  cases h₅,
  -- c = a
  rewrite ← h₅,
  rewrite nat.add_sub_cancel_left,
  rewrite nat.sub_eq_zero_of_le,
  apply nat.zero_lt_one,
  ----
  apply nat.succ_le_of_lt; assumption,
  -- c < a
  have h₆ := nat.lt_or_ge a b,
  cases h₆,
  -- a < b
  have h₇ : a + 1 ≤ b,
    apply nat.succ_le_of_lt; assumption,
  have rewrite_l : a + 1 - b = 0,
    apply nat.sub_eq_zero_of_le; assumption,
  rewrite rewrite_l,
  rewrite nat.add_comm,
  rewrite nat.add_sub_assoc,
  have temp : 1 + (a - c) = nat.succ (a - c),
    rewrite nat.add_comm,
  rewrite temp,
  apply nat.zero_lt_succ,
  assumption,
  -- a ≥ b
  rewrite nat.add_comm,
  rewrite nat.add_sub_assoc,
  rewrite nat.add_sub_assoc,
  apply add_lt_add_left,
  apply ih,
  repeat {assumption}
end

lemma sub_le :
  ∀ n m : ℕ, n - m ≤ n
| 0 m := by simp
| n 0 := by simp
| (n+1) (m+1) :=
begin
  rewrite nat.add_comm,
  simp,
  apply nat.le_succ_of_le,
  apply sub_le
end

lemma sub_succ :
  ∀ n : ℕ,
  1 ≤ n →
  n - 1 + 1 = n
| 0 h := by cases h
| (n+1) h := by simp

lemma one_le_of_add_le :
  ∀ n m : ℕ,
  1 + m ≤ n →
  1 ≤ n
| n 0 h := by simp at *; assumption
| n (m+1) h :=
begin
  apply one_le_of_add_le n m,
  apply nat.le_of_succ_le,
  assumption
end

lemma succ_le_le_sub :
  ∀ n m : ℕ,
  n + 1 ≤ m →
  n ≤ m - 1
| n 0 h := by cases h
| n (m+1) h :=
begin
  simp,
  rewrite ← nat.add_le_add_iff_le_right,
  assumption
end

lemma smaller_sub :
  ∀ n a b : ℕ,
  a < b → a < n → n - b < n - a 
| 0 a b h1 h2 := by cases h2
| n a 0 h1 h2 := by cases h1
| n 0 b h1 h2 :=
begin
  simp,
  cases nat.lt_or_ge n b,
    rewrite nat.sub_eq_zero_of_le,
    assumption,
    apply nat.le_of_lt h,
    ---
    apply nat.sub_lt_of_pos_le,
    assumption,
    assumption,
end
| (n+1) (a+1) (b+1) h1 h2 :=
begin
  repeat {rewrite nat.add_sub_add_right},
  apply smaller_sub,
  rewrite nat.add_comm at h1,
  rewrite nat.add_comm b 1 at h1,
  apply nat.lt_of_add_lt_add_left; assumption,
  rewrite nat.add_comm at h2,
  rewrite nat.add_comm n 1 at h2,
  apply nat.lt_of_add_lt_add_left; assumption,
end

lemma sub_le_of_sub_le_succ_of_lt :
  ∀ n m a b : ℕ,
  a < b →
  n - a ≤ m + 1 →
  n - b ≤ m
| n m a b h₁ h₂ :=
begin
  cases nat.lt_or_ge b n,
  -- b < n
  have h' := nat.lt_trans h₁ h,
  have h'' := smaller_sub n a b h₁ h',
  apply nat.le_of_lt_succ,
  cases nat.eq_or_lt_of_le h₂ with heq hlt,
    rewrite heq at h'',
    apply h'',
    ---
    apply nat.lt_trans h'' hlt,
  -- b ≥ n
  rewrite nat.sub_eq_zero_of_le,
  apply nat.zero_le,
  assumption
end

lemma lt_sub_succ_n :
  ∀ n m k : ℕ,
  n - m = k + 1 → m < n
| n m k h :=
begin
  cases nat.lt_or_ge m n,
  assumption,
  rewrite nat.sub_eq_zero_of_le at h,
  cases h,
  assumption
end

lemma one_add :
  ∀ n : ℕ,
  nat.succ n = 1 + n
| n := by rewrite ← nat.add_one; simp

lemma only_zero_lt_one :
  ∀ n : ℕ,
  n < 1 → n = 0
| 0 h := by reflexivity
| (n+1) h :=
begin
  simp at h,
  cases h
end

lemma mul_le_inside :
  ∀ a b n m : ℕ,
  a ≤ b →
  n * b ≤ m →
  n * a ≤ m
| a b n m h1 h2 :=
begin
  have h3 := nat.mul_le_mul_left n h1,
  apply nat.le_trans,
  assumption,
  assumption
end

lemma diff_ge_1 :
  ∀ n m : ℕ,
  n < m → 1 ≤ m - n
| n m h :=
begin
  -- have h' := nat.le_of_lt_succ h,
  rewrite ← nat.add_le_add_iff_le_right n,
  rewrite nat.sub_add_cancel,
  -- rewrite hm,
  apply nat.le_of_lt_succ,
  rewrite ← nat.add_one,
  rewrite nat.add_comm,
  apply nat.add_lt_add_right,
  assumption,
  apply nat.le_of_lt,
  assumption
end

end jitsynth