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

This document is a work-in-progress report containing a detailed description of
the bidirectional typechecker implemented in the MiniJuvix project. The primary
purpose is to serve as a guide to extending the Juvix typechecker.



## Core syntax

The type theory implemented in MiniJuvix is quantitative type theory (QTT),
where each term has a usage/quantity annotation in the semiring from
$\{0,1,\omega\}$ using the order $0<\omega$, $1<\omega$, and $0 \not < 1$. The
core language in MiniJuvix is bidirectional syntax-based, meaning that a term in
the language is either a checkable term or an inferable term. We therefore find
two AST right below for each case.


\begin{aligned}
x,y,z &\EQ \dotsb & \text{term variables} \\[.5em]
\pi,\rho,\sigma &\EQ 0 \Or 1 \Or \omega
  & \text{quantity variables} \\[.5em]
s, t, A, B &\EQ \mathcal{U} & \text{Universe type} \\
           &\OR (x :^{\sigma} A) \to B      &\Pi\mbox{-}\text{types} \\
           %&\OR ...      &\text{...} \\[.5em]
e, f &\EQ  & \text{... } \\
     &\OR ...  & \text{... }
\\[1em]
\Gamma, \Delta &\EQ \emptyset \Or \Gamma, x :^{\sigma} A & \text{ contexts}
\end{aligned}

TODO: add all the cases based on the types below.

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

# Judgements

$$
\begin{gathered}
 \Gamma \vdash t \Leftarrow^\sigma M \text{   (type checking)}
\\[.5em]
\Gamma \vdash t \Rightarrow^\sigma M \text{   (type inference)}
\end{gathered}
%
$$

- [ ] Explain the usage/resource semantics.

## Contexts

# Type checking

- [ ] UniverseType
- [ ] PiType
- [ ] Lam
- [ ] TensorType
- [ ] TensorIntro
- [ ] UnitType
- [ ] Unit 
- [ ] SumType


# Type inference


The algorithm that implements type inference is called `infer`. The `infer`
method receives three arguments: one implicit argument for the context $\Gamma$
and two explicit arguments, respectively, the term $t$ and its quantity $\sigma$
in the rule below. The output of the algorithm is precisely the type $M$ for
$t$.

$$
\begin{gathered}
\rule{}{
p_1 \cdots\ p_n
}{
 \Gamma \vdash t \Rightarrow^\sigma M 
}
\end{gathered}
%
$$

The variable $M$ in the rule above represents the output of the algorithm. The
variables $p_i$ are inner steps of the algorithm and their order is relevant. An
inner step can be infering a type, checking if a property holds, reducing a
term, or checking a term against a type. A reduction step is denoted by $\Gamma
\vdash t \rightsquigarrow t'$ or simply by $t \rightsquigarrow t'$ whenever the
context $\Gamma$ is known. Such a reduction is obtained by calling `eval` in the
implementation.

By design, a term is inferable if it is one of the following cases.

- [x] Variables
- [x] Annotations
- [x] Applications
- [ ] Tensor type elim
- [ ] Sum type elim


Each case above has as a rule in what follows. 

The Haskell type of `infer` would be similar as the following.

```haskell
infer :: Quantity -> InferableTerm -> Output (Type , Resources)
```

where

```haskell
Output = Either ErrorType 
Resources = Map Name Quantity
```

## Variables

A variable can be *free* or *bound*. If the variable is free, the rule is as
follows.

### Free variables

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

*Explanation*:

1. The input to `infer` is a variable term of the form `Free x`.
2. The only case for introducing a variable is to have it in the context.
3. Therefore, we ask if the variable is in the context.
4. If it's not the case, throw an error.
5. Otherwise, one gets a hypothesis $x :^\sigma S$ from the context that matches $x$.
6. At the end, we return two things: 
  6.1. first, the inferred type and
  6.2. a table with the new usage information for  each variable.
  
Haskell prototype:

```haskell
infer σ (Free x) = do
  Γ <- asks contextMonad
  case find ((== x) . getVarName) Γ of
    Just (BindingName _ _σ typeM) 
      -> return (typeM, updateResources (x, _σ) )
    Nothing               
      -> throwError "Variable not present in the context"
```

The method `updateResources` rewrites the map tracking names with their quantities.

### Bound variables

The case of the`Bound` variable throws an error.


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

- First, we must check that $M$ is a type, i.e., a term in *some* universe.
Because there is only one universe we denote it by $\mathcal{U}$. The formation
rule for types has no computation content, then the usage is zero in this case.
- Second, the term $x$ needs to be checked against $M$ using the same usage
$\sigma$ we need in the conclusion. The context for this is $\Gamma$. There is
one issue here. This type checking expects $M$ to be in normal form. When it is
not, typechecking the judgment $\Gamma \vdash x \Leftarrow^\sigma M$ may give us
a false negative.


    - *Example*: Why do we need $M'$? Imagine that we want to infer the type of $v$ given $\Gamma \vdash x : \mathsf{Ann}(v, \mathsf{Vec}(\mathsf{Nat},2+2))$. Clearly, the answer should be `Vec(Nat,4)`. However, this reasoning step requires computation. $$\Gamma \vdash x : \mathsf{Ann}(v, \mathsf{Vec}(\mathsf{Nat},2+2)) \Rightarrow \mathsf{Vec}(\mathsf{Nat},4))\,.$$
     
-  Using $M'$ as the normal form of $M$, it remains to check if $x$ is of type
$M'$. If so, the returning type is $M'$ and the resources map has to be updated
(the $\color{gray}{gray}$ $\Theta$ in the rule below).

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

Haskell prototype:

```haskell
infer _ (Ann termX typeM) = do
  _         <- check (0 .*. context) typeM zero Universe
  typeM'    <- evalWithContext typeM
  (_ , newUsages) <- check context termX typeM'
  return (typeM' , newUsages)
```

## Applications

Recall the task is to find $M$ in $\Gamma \vdash \mathsf{App}(f,x) :^{\sigma}
M$. If we follow the bidirectional type-checking recipe, then it makes sense to
infer the type for an application, i.e., $\Gamma \vdash \mathsf{App}(f,x)
\Rightarrow^{\sigma} M$. An application essentially removes a lambda abstraction
introduced earlier in the derivation tree. The rule for this inference case is a
bit more settle, especially because of the usage variables.

To introduce the term of an application, $\mathsf{App}(f,x)$, it requires to
give/have a judgement saying that $f$ is a (dependent) function, i.e., $\Gamma
\vdash f :^{\sigma} (x : ^\pi A) \to  B$, for usages variables $\sigma$ and
$\pi$. Then, given $\Gamma$, the function $f$ uses $\pi$ times its input,
mandatory. We therefore need $\sigma\pi$ resources of an input for $f$ if we
want to apply $f$ $\sigma$ times, as in the conclusion $\Gamma \vdash
\mathsf{App}(f,x) \Rightarrow^{\sigma} M$.

In summary, the elimanation rule is as follows.

$$\begin{gathered}
\rule{}{
\Gamma \vdash f :^{\sigma} (x : ^\pi A) \to  B
\qquad
\sigma\pi\cdot\Delta \vdash x : ^{\sigma\pi} A
}{
\Gamma + \sigma\pi\cdot\Delta  \vdash f\,x :^{\sigma} B
}
\end{gathered}
%
$$


The first judgement about $f$ is *principal*. Then, it must an inference step.
After having inferred the type of $f$, the types $A$ and $B$ become known facts.
It is then time to check the type of $x$ against $A$. 

$$\begin{gathered}
\rule{App{\Rightarrow_2}}{
\Gamma \vdash f {\color{blue}\Rightarrow}^{\sigma}(x : ^\pi A) \to  B
\qquad
\sigma\pi\cdot\Delta \vdash x {\color{red}\Leftarrow}^{\sigma\pi} A
}{
\Gamma + \sigma\pi\cdot\Delta  \vdash f\,x\,{\color{blue}\Rightarrow^{\sigma}}\, B
}
\end{gathered}
%
$$


To make our life much easier, the rule above can be splitted in two cases,
emphasising the usage bussiness.


1. $$\begin{gathered}
\rule{App{\Rightarrow_1}}{
\Gamma \vdash f {\color{blue}\Rightarrow^{\sigma}} (x :^{\pi} A) \to B
\qquad
\color{green}{\sigma \cdot \pi = 0}
\qquad
0\Gamma \vdash x {\color{red}\Leftarrow^{0}} A
}{
\Gamma \vdash f\,x \Rightarrow^{\sigma} B
}
\end{gathered}
%
$$

2. $$\begin{gathered}
\rule{App{\Rightarrow_2}}{
\Gamma_1 \vdash f \Rightarrow^{\sigma} (x :^{\pi} A) \to B
\qquad
\color{green}{\sigma \cdot \pi \neq 0}
\qquad
\Gamma_2 \vdash x \Leftarrow^{1} A
}{
\Gamma_1 + \sigma \pi\cdot \Gamma_2 \vdash f\,x \Rightarrow^{\sigma} B
}
\end{gathered}
$$

In the rules above, we have used two lemmas:

- $1 \cdot \Gamma \vdash x :^1 M$ entails that $\rho \cdot \Gamma \vdash x
:^\rho M$ for any usage $\rho$.


In summary, we infer the type of $f$. If it is a $\Pi$-type, then one checks
whether $\sigma\pi$ is zero or not. If so, we use Rule No.1, otherwise, Rule No.
2. Otherwise, something goes wrong, an error arise.


Sketch:

```haskell=
infer σ (App f x) = do
  (arrowAtoB, usages) <- infer σ f
  case arrowAtoB of
    IsPiType π _ typeA typeB -> do
      σπ <- case (σ .*. π) of
       -- Rule No. 1
       Zero -> do 
         (_ , nqs) <- check x typeA (mult Zero context)
          return nqs
       -- Rule No. 2
       _ -> undefined -- TODO (mult σπ context)
    -- f is not a function:
    ty -> throwError $ Error ExpectedPiType ty (App f x)
```

## Tensor type elim

At this point and following the previous case, it makes sense why we need to
infer the type and not to check if an elimination rule is studied.

## Sum type elim

TODO
