import Juvix.Core.Main
open Juvix.Core.Main

lemma step_0_constant_folding : Expr.save (Expr.binop BinaryOp.add_int (Expr.binop BinaryOp.mul_int (Expr.const (Constant.int 3)) (Expr.const (Constant.int 2))) (Expr.const (Constant.int 7))) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  unfold Expr.Equiv Expr.Equiv.Param Expr.Approx.Param
  intro env₁ env₂ henv
  constructor
  case left =>
    intro v₁ heval
    exists v₁
    constructor
    case left =>
      cases heval
      case save val heval heval' =>
        cases heval'
        case var hvar =>
          simp_all
          subst hvar
          cases heval
          case binop i₁ i₂ heval₁ heval₂ =>
            cases heval₂
            case const =>
              cases heval₁
              case binop i₁ i₂ heval₁ heval₂ =>
                cases heval₁
                case const =>
                  cases heval₂
                  case const =>
                    repeat constructor
        case var_rec =>
          aesop
    case right =>
      rfl
  case right =>
    intro v₁ heval
    exists v₁
    constructor
    case left =>
      cases heval
      case save val heval heval' =>
        cases heval'
        case var hvar =>
          simp_all
          subst hvar
          cases heval
          case const =>
            repeat constructor
        case var_rec =>
          aesop
    case right =>
      rfl

lemma step_1_inlining : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_2_simplification : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_3_specialize_args : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_4_simplification : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_5_constant_folding : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_6_inlining : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_7_simplification : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_8_specialize_args : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_9_simplification : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_10_constant_folding : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_11_inlining : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_12_simplification : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_13_specialize_args : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_14_simplification : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_15_constant_folding : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_16_inlining : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_17_simplification : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_18_specialize_args : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_19_simplification : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl

lemma step_20_constant_folding : Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) ≈ Expr.save (Expr.const (Constant.int 13)) (Expr.var 0) := by
  rfl
