# MiniJuvix typechecker WIP

<!-- Latex stuff adapted from Andy's record proposal: https://hackmd.io/Xlvu82eFQ_m-cebUWAtGsw?edit -->

$$
\renewcommand\.{\mathord.}
\newcommand\EQ{\mkern5mu\mathrel{::=}}
\newcommand\OR[1][]{\mkern17mu | \mkern12mu}
\newcommand\Or{\mathrel|}
\newcommand\RT[1]{\{#1\}}
\newcommand\RV[1]{\langle#1\rangle}
\newcommand\Let{\mathbf{let}\:}
\newcommand\In{\:\mathbf{in}\:}
\newcommand\Return{\:\mathbf{return}\:}
\newcommand\Q{\mathrel|}
\newcommand\I{\color{blue}} \newcommand\O{\color{green}}
\let\Rule\mathsf
\let\N\mathsf
\newcommand\rule[3]{\frac{\begin{gathered}#2\end{gathered}}{#3}\:\Rule{#1}}
\let\Check\Leftarrow 
\let\Infer\Rightarrow
\newcommand\Tel[1]{\mathbf{tel}\:#1}
\newcommand\Type[1]{\star_{#1}}
$$

###### tags: `juvix-project`, `MiniJuvix`

This document is a work-in-progress report containing a detailed description of the bidirectional typechecker implemented in the MiniJuvix project. The primary purpose is to serve as a guide to extending the Juvix typechecker.

## Core syntax

The type theory implemented in MiniJuvix is, at the time of writing, Quantitative type theory, i.e., each term has a usage/quantity annotation in the semiring from $\{0,1,\omega\}$ using the order $0<\omega$, $1<\omega$, and $0 \not < 1$. Furthermore, the core language is bidirectional syntax-based. As a consequence, any term in the language is either a checkable term or an inferable term, and each case has its own AST. 


\begin{aligned}
x,y,z &\EQ \dotsb & \text{term variables} \\[.5em]
\pi,\rho,\sigma &\EQ 0 \Or 1 \Or \omega
  & \text{quantity variables} \\[.5em]
s, t, A, B &\EQ \mathcal{U} & \text{Universe type} \\
           &\OR (x :^{\sigma} A) \to B(x)      &\Pi\mbox{-}\text{types} \\
           %&\OR ...      &\text{...} \\[.5em]
           &\OR ...      &\text{...} \\[.5em]
C, D &\EQ \mathcal{U} & \text{Universe type} \\
           &\OR ... & ... \\
           &\OR ...      &\text{...} \\[.5em]           
e, f &\EQ  & \text{... } \\
     &\OR ...  & \text{... }
\\[1em]
\Gamma &\EQ \emptyset \Or \Gamma, x :^{\sigma} A & \text{ contexts}
\end{aligned}

### Checkable terms


```haskell
data CheckableTerm where
  {- Universe types. 
  See the typing rule Univ⇐.
  -}
  UniverseType : CheckableTerm
  {- Dependent function types. 
  See the typing rules →F⇐ and →I⇐.
    1. (Π[ x :ρ S ] P x) : U
    2. (λ x. t) : Π[ x :ρ S ] P x
  -}
  PiType : Quantity → BindingName → CheckableTerm 
         → CheckableTerm → CheckableTerm
  Lam : BindingName → CheckableTerm → CheckableTerm
  {- Dependent tensor product types. 
  See the typing rules ⊗-F-⇐,  ⊗-I₀⇐, and ⊗-I₁⇐.
    1. * S ⊗ T : U
    2. (M , N) : S ⊗ T
  -}
  TensorType : Quantity → BindingName → CheckableTerm 
             → CheckableTerm → CheckableTerm
  TensorIntro : CheckableTerm → CheckableTerm → CheckableTerm
  {- Unit types. 
  See the typing rule 1-F-⇐ and 1-I-⇐.
    1. 𝟙 : U
    2. ⋆ : 𝟙
  -}
  UnitType : CheckableTerm
  Unit : CheckableTerm
  {- Disjoint sum types.
  See the typing rules
    1. S + T : U
    2. inl x : S + T
    3. inr x : S + T
  -}
  SumType : CheckableTerm → CheckableTerm → CheckableTerm
  Inl : CheckableTerm → CheckableTerm
  Inr : CheckableTerm → CheckableTerm
  -- Inferrable terms are clearly checkable, see typing rule Inf⇐.
  Inferred : InferableTerm → CheckableTerm
```

### Inferable terms

```haskell
data InferableTerm where
  -- | Variables, typing rule Var⇒. 
  Var : Variable → InferableTerm
  -- | Annotations, typing rule Ann⇒.
  {- Maybe, I want to have the rules here like this:
  
    OΓ ⊢ S ⇐0 𝕌     Γ ⊢ M ⇐0 𝕌
    ────────────────────────────── Ann⇒
           Γ ⊢ (M : S) ⇒ S
  -}
  Ann : CheckableTerm → CheckableTerm → InferableTerm
  -- |  Application (eliminator).
  App : InferableTerm → CheckableTerm → InferableTerm
  -- | Dependent Tensor product eliminator. See section 2.1.3 in Atkey 2018.
  -- let z@(u, v) = M in N :^q (a ⊗ b))
  TensorTypeElim
    : Quantity       -- q is the multiplicity of the eliminated pair.
    → BindingName          -- z is the name of the variable binding the pair in the
                     -- type annotation of the result of elimination.
    → BindingName          -- u is the name of the variable binding the first element.
    → BindingName          -- v is the name of the variable binding the second element.
    → InferableTerm  -- (u,v) is the eliminated pair.
    → CheckableTerm  -- Result of the elimination.
    → CheckableTerm  -- Type annotation of the result of elimination.
    → InferableTerm
  -- | Sum type eliminator (a.k.a. case)
  -- let (z : S + T) in (case z of {(inl u) ↦ r1; (inr v) ↦ r2}  :^q  T) 
  SumTypeElim        -- Case
    :  Quantity      -- Multiplicity of the sum contents.
    →  BindingName         -- Name of the variable binding the sum in the type
                     -- annotation of the result of elimination.
    → InferableTerm  -- The eliminated sum.
    → BindingName          -- u is the name of the variable binding the left element.
    → CheckableTerm  -- r1 is the result of the elimination in case the sum contains
                     -- the left element.
    → BindingName          -- v is the name of the variable binding the right element.
    → CheckableTerm  -- r2 is the result of the elimination in case the sum contains
                     -- the right element.
    → CheckableTerm  -- Type annotation of the result of the elimination.
    → InferableTerm
```


### Notation


- [x] The reduction for a term $t$ in the context $\Gamma$ is denoted by $\Gamma \vdash t \rightsquigarrow t'$ for some $t'$, or simply by $t \rightsquigarrow t'$ if the context $\Gamma$ can be inferred. Such a reduction is in the implementation a call to the method `eval`, which also calls to the method `evalWithContext` with the global context.

Each case of the bidirectional typechecker is presented as a rule. Each rule represents one possible combination, in the sense that there may exist coherent order. The order is relevant from the implementation point of view.

$$
\begin{gathered}
\rule{name}{
p_1 \qquad p_2 \quad  \cdots \quad p_n
}{
c
}
\end{gathered}
%
$$


# Type inference (a.k.a synthesis)

For type inference, the following cases need to be addressed.

- [x] Variables
- [x] Annotations
- [x] Applications
- [ ] Tensor type elim
- [ ] Sum type elim

## Variables


$$
\begin{gathered}
\rule{Var⇒}{
(x :^{\sigma} M) \in \Gamma
}{
  \Gamma \vdash \mathsf{Free}(x) \Rightarrow^{\sigma} M
}
\end{gathered}
%
$$

### Free variables

1. The input to `infer` is a variable term of the form `Free x`.
2. The only case for introducing a variable is to have it in the context.
3. Therefore, we ask if the variable is in the context.
4. If it's not the case, throw an error.
5. Otherwise, one gets a hypothesis $x :^q S$ from the context that matches $x$.
6. At the end, we return two things: 
  6.1. first, the inferred type and
  6.2. a table with the new usage information for  each variable.
  
Sketch:

```haskell
infer : Relevance -> InferableTerm -> Either ErrorType (Type , Map Name Quantity)
infer relevance (Free x) = do
  Γ <- asks contextMonad
  case find ((== x) . getVarName) Γ of
    Just (BindingName _ σ typeM) 
      -> return (typeM, updatedLeftOvers (x, relevance) )
    Nothing               
      -> throwError "Variable not present in the context"
```

### Bound variables

The case of the`Bound` variable just throws an error.


## Annotations

$$
\begin{gathered}
\rule{Ann⇒}{
0\Gamma \vdash M \Leftarrow^0 \mathcal{U}
\qquad
\Gamma \vdash x \Leftarrow^\sigma M
}{
  \Gamma \vdash \mathsf{Ann}(x,M) \Rightarrow^{\sigma} M
}
\end{gathered}
%
$$

An annotation is something we infer, this is a choice.

- First, we must check that $M$ is a type, i.e., a term of *some* universe. At the time of writing, only one universe is available and we denote it by $\mathcal{U}$.
- Second, one has to check if $x$ is of type $M$ given the context $\Gamma$. To do this check, we need $M$ to be in normal form, otherwise, the check may give us a false negative. We denote by $M'$ the normal form of $M$. 


    - *Example*: Why do we need $M'$? Imagine that we want to infer the type of $v$ given $\Gamma \vdash x : \mathsf{Ann}(v, \mathsf{Vector}(\mathsf{Nat},2+2))$. Clearly, the answer should be `Vector(Nat,4)`, but this output requires a computation step, therefore, we work with $M$, instead of $M'$.
     
- Finally, it remains to check if $x$ is of type $M'$. If so, we also return the new usage table.

$$
\begin{gathered}
\rule{Ann⇒}{
0\Gamma \vdash M \Leftarrow^0 \mathcal{U}
\qquad
M \rightsquigarrow M'
\qquad
\Gamma \vdash x \Leftarrow^\sigma M' \color{darkgrey}{\dashv \Theta}
}{
  \Gamma \vdash \mathsf{Ann}(x,M) \Rightarrow^{\sigma} M' \color{darkgrey}{\dashv \Theta}
}
\end{gathered}
%
$$

Sketch:

```haskell
infer _ (Ann termX typeM) = do
  _         <- check (zeroQuantity context) typeM zero Universe
  typeM'    <- evalWithContext typeM
  newUsages <- check context termX typeM'
  return (typeM' , newUsages)
```

## Applications

Following the bidirectional type-checking recipe it makes sense to infer the type in an application. Recall that an application essentially removes a lambda abstraction, as it is precisely its elimination rule. 

One needs to find $M$ in $\Gamma \vdash \mathsf{App}(f,x) : M$. The only way is to infer a type for $f$ because the following two reasons. First, we clearly don't know a priori a type to check for $f$, we just know if must be a $\Pi$-type. Second, this case is what the recipe calls the *principal judgement*. Then, the corresponding rule must synthetise a type, as Then, it is eliminating the arrow symbol.

Under these considerations, two scenarios arise. The treatment of usages in each follows the intuition given by the Atkey's paper. In first case, note that after infering the type of $f$, the types $A$ and $B$ become known facts. Therefore, it is right to check that $x$ is of type $A$, see the red symbol below. 

1. $$\begin{gathered}
\rule{App{\Rightarrow_2}}{
\Gamma \vdash f {\color{blue}\Rightarrow^{\sigma}} (x :^{\pi} A) \to B
\qquad
\sigma \cdot \pi = 0
\qquad
0\Gamma \vdash x {\color{red}\Leftarrow^{0}} A
}{
\Gamma \vdash f\,x \Rightarrow^{\sigma} B
}
\end{gathered}
%
$$

1. $$\begin{gathered}
\rule{App{\Rightarrow_1}}{
\Gamma_1 \vdash f \Rightarrow^{\sigma} (x :^{\pi} A) \to B
\qquad
\sigma \cdot \pi \neq 0
\qquad
\Gamma_2 \vdash x \Leftarrow^{1} A
}{
\Gamma_1 + (\sigma \cdot \pi)\, \Gamma_2 \vdash f\,x \Rightarrow^{\sigma} B
}
\end{gathered}
$$


Sketch:

```haskell=
infer σ (App f x) = do
  (arrowAtoB, usages) <- infer σ f
  case arrowAtoB of
    IsPiType π _ typeA typeB -> do
      σπ <- case (σ .*. π) of
       Zero -> do 
         (_ , nqs) 
             <- check x typeA (mult Zero context)
          return nqs
       _ -> TODO: (mult σπ context)
    -- f is not a function:
    ty -> throwError $ Error ExpectedPiType ty (App f x)
```

## Tensor type elim

At this point and following the previous case, it makes sense why we need to infer the type and not to check if an elimination rule is studied.

## Sum type elim

TODO


# Type checking

- [ ] UniverseType
- [ ] PiType
- [ ] Lam
- [ ] TensorType
- [ ] TensorIntro
- [ ] UnitType
- [ ] Unit 
- [ ] SumType