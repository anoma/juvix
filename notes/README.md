# Proposal: MiniJuvix

<!-- Latex stuff adapted from Andy's record proposal: 
https://hackmd.io/Xlvu82eFQ_m-cebUWAtGsw?edit -->

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
\newcommand\rule[3]{\frac{\begin{gathered}\,{#2}\end{gathered}}{#3}\:\Rule{#1}}
\let\Check\Leftarrow 
\let\Infer\Rightarrow
\newcommand\Tel[1]{\mathbf{tel}\:#1}
\newcommand\Type[1]{\star_{#1}}
$$

###### tags: `juvix-project`, `MiniJuvix`

## Abstract 

This document is a **work-in-progress** report containing a detailed description of
the bidirectional typechecker implemented in the MiniJuvix project. The primary
purpose is to serve as a guide to extending the Juvix typechecker.

## Core syntax

The type theory implemented in MiniJuvix is quantitative type theory (QTT),
where each term has a usage/quantity annotation in the semiring from
$\{0,1,\omega\}$ using the order $0<\omega$, $1<\omega$, and $0 \not < 1$. The
core language in MiniJuvix is bidirectional syntax-based, meaning that a term in
the language is either a checkable term or an inferable term. We therefore find
two AST right below for each case.

### Terms and types

The following syntax description is given
thinking on its implementation. This may change later, for presentation purposes. The gray parts are
expected to be added later.

<!-- \begin{aligned}
%x,y,z &\EQ \dotsb & \text{term variable} \\[.5em]
\pi,\rho,\sigma &\EQ 0 \Or 1 \Or \omega
  & \text{quantity variables} \\[.5em]
A, B &\EQ \mathcal{U}                  & \text{universe} \\
     &\OR (x :^{\sigma} A) \to B       &\text{depend. fun. type} \\
     &\OR (x :^{\sigma} A) \otimes B   &\text{tensor prod. type} \\[.5em]
u, v &\EQ \mathsf{Var}(x)    &\text{variable}\\
&\OR \mathsf{Ann}(x, A) &\text{annotation}\\

&\OR u\,v  &\text{application} \\
&\OR \lambda x.v  &\text{lambda abs.} \\[.5em]
&\OR \color{gray}{f}  &\color{gray}{\text{named function(review)}} \\
&\OR \color{gray}{D} &\color{gray}{\text{data type decl.}}\\
&\OR \color{gray}{c} &\color{gray}{\text{data constr.}}\\
&\OR \color{gray}{R}  &\color{gray}{\text{...check andy's record constr decl..}} \\
&\OR \color{gray}{r}  &\color{gray}{\text{record. intro}} \\ 
&\OR \color{gray}{\cdots}  &\color{gray}{\text{record. proj.}} \\[.7em]
\Gamma &\EQ \emptyset \Or \Gamma, x :^{\sigma} A & \text{ contexts}
\\[1em]
\Delta &\EQ () \Or (x : A)\,\Delta& \text{ telescopes}
\end{aligned} -->

TODO: add all the cases based on the types below. Mention something about metavariables, as they appear usually in the context of type inference. Introduce telescopes for data types later and for pattern-matching.

## Judgement forms


$$
\begin{gathered}
 \Gamma \vdash t\, {\color{red}\Leftarrow}^\sigma\, M \text{   (type checking)}
\\[.5em]
\Gamma \vdash t \, {\color{blue} \Rightarrow}^\sigma \, M \text{   (type inference)}
\end{gathered}
%
$$


## Contexts

- The formation rules for contexts.
- Context transformations

## Type checking

TODO: explain the intuition about the following rules below.

- [x] UniverseType
- [x] PiType
- [x] Lam
- [x] TensorType
- [ ] TensorIntro
- [ ] UnitType
- [ ] Unit 
- [ ] SumType

There is no resources needed to form a type. Therefore, the rules
below only checks when forming a type, both, in the premises and the conclusion.

### Universe

- Type formation rule

$$\begin{gathered}
\rule{Univ{\Leftarrow}}{
0\cdot \Gamma \vdash  
}{
0\cdot \Gamma \vdash \mathcal{U}\, \overset{0}{\color{red}\Leftarrow}\,\, \mathcal{U}
}
\end{gathered}
%
$$

### Dependent function types

- Type formation rule

$$\begin{gathered}
\rule{Pi{\Leftarrow}}{
0\cdot \Gamma \vdash A \, \overset{0}{\color{red}\Leftarrow}\,\mathcal{U}
\qquad
0\cdot \Gamma,\,x\overset{0}{:}A \vdash B \, \overset{0}{\color{red}\Leftarrow}\,\mathcal{U}\quad
}{
0\cdot \Gamma \vdash (x\overset{\pi}{:}A)\rightarrow B \overset{0}{\color{red}\Leftarrow}\mathcal{U}
}
\end{gathered}
%
$$

### Lambda abstractions

The lambda abstraction rule is the introduction rule of a dependent function
type. The principal judgement in this case is the conclusion, and we therefore
check against the corresponing type $(x \overset{\sigma}{:} A) \to B$. With
the types $A$ and $B$, all the information about the premise
becomes known, and it just remains to check its judgment.



$$\begin{gathered}
\rule{lam{\Leftarrow}}{
\Gamma, x\overset{\sigma\pi}{:}A \vdash M \,\overset{\sigma}{\color{red}\Leftarrow}\,B
}{
\Gamma \vdash \lambda x.M \overset{\sigma}{\color{red}\Leftarrow} (x\overset{\pi}{:}A)\rightarrow B
}
\end{gathered}
%
$$

### Tensor product types

- Type formation rule

$$\begin{gathered}
\rule{\hspace{0.4cm}\otimes-F{\Leftarrow}}{
0\cdot \Gamma \vdash A \,\overset{0}{\color{red}\Leftarrow}\,\mathcal{U}
\qquad
0\cdot \Gamma, x\overset{0}{:}A \vdash B\overset{0}{\color{red}\Leftarrow}\,\mathcal{U}
}{
0\cdot \Gamma \vdash (x\overset{\pi}{:}A) \otimes B \overset{0}{\color{red}\Leftarrow}\,\mathcal{U}
}
\end{gathered}
%
$$

- Make pairs

The original rule in Atkey's paper is too condense. A more didactical way on
presenting the rules is splitting it into two cases: the erased and present
part of the theory, i.e., respectively, $\sigma =0$ or $\sigma \neq 0$ in the
rule's conclusion.


TODO: use the same reasoning as for applications.


1. 

$$\begin{gathered}
\rule{TensorIntro_1{\Leftarrow}}{
\color{green}{\sigma\pi = 0}
\qquad
0\cdot \Gamma \vdash M \,\overset{0}{\color{red}\Leftarrow}\,A
\qquad
\Gamma \vdash N \,\overset{\sigma}{\color{red}\Leftarrow}\,B[M/x]
\qquad 
}{
\Gamma \vdash (M,N)\overset{\sigma}{\color{red}\Leftarrow} (x\overset{\pi}{:}A) \otimes B
}
\end{gathered}
%
$$

2. 

$$\begin{gathered}
\rule{TensorIntro_2{\Leftarrow}}{
\color{green}{\sigma\pi \neq 0}\qquad
\Gamma_{1} \vdash M \,\overset{1}{\color{red}\Leftarrow}\,A
\qquad
\Gamma_{2} \vdash N \,\overset{\sigma}{\color{red}\Leftarrow}\,B[M/x]
\qquad 
}{
\color{green}{\sigma\pi}\cdot \Gamma_{1}+\Gamma_{2} \vdash (M,N)\overset{\sigma}{\color{red}\Leftarrow} (x\overset{\pi}{:}A) \otimes B 
}
\end{gathered}
%
$$



## Conversion rules


Include the rules for definitional equality:
- [ ] β-equality, 
- [ ] reflexivity, 
- [ ] symmetry, 
- [ ] transitivity, and
- [ ] congruence.


We want to check if $M$ is of type $T$.

$$\begin{gathered}
\rule{Inf{\Leftarrow}}{
\Gamma \vdash M \,\overset{\sigma}{\color{blue}\Rightarrow}\,S \qquad
\Gamma \vdash S\, \overset{0}{\color{red}\Leftarrow}\, \mathcal{U}\qquad
\Gamma \vdash T \,\overset{0}{\color{red}\Leftarrow}\,\mathcal{U} \qquad 
\color{green}{S =_{\beta} T}\ \,\,\,
}{
\Gamma \vdash M \overset{\sigma}{\color{red}\Leftarrow} T
}
\end{gathered}
%
$$


# Type inference

The algorithm that implements type inference is called `infer`. Inspired by Agda and its inference strategy, MiniJuvix only infer values that are *uniquely* determined by the context.
There are no guesses. Either we fail or get a unique answer, giving us a predicatable behaviour. 

The `infer`
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

The variables $p_i$ in the rule are inner steps of the algorithm and their order
is relevant. An inner step can be infering a type, checking if a property holds,
reducing a term, or checking a term against a type. A reduction step is denoted
by $\Gamma \vdash t \rightsquigarrow t'$ or simply by $t \rightsquigarrow t'$
whenever the context $\Gamma$ is known. Such a reduction is obtained by calling
`eval` in the implementation.

By design, a term is inferable if it is one of the following cases.

- [x] Variable
- [x] Annotation
- [x] Application
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

## Variable

A variable can be *free* or *bound*. If the variable is free, the rule is as
follows.

### Free variable

$$
\begin{gathered}
\rule{Var⇒}{
(x :^{\sigma} M) \in \Gamma
}{
  \Gamma \vdash \mathsf{Free}(x) {\color{blue}\Rightarrow}^{\sigma} M
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
\rule{Ann{⇒}}{
0\cdot \Gamma \vdash M\,{\color{red}\Leftarrow}^0\,\mathcal{U}
\qquad
\Gamma \vdash x\,{\color{red}\Leftarrow}^\sigma\, M
}{
  \Gamma \vdash \mathsf{Ann}(x,M)\,{\color{blue}\Rightarrow}^{\sigma}\,M
}
\end{gathered}
%
$$

Any annotation possess type information that counts as known facts, and we therefore infer. However, this is a choice.

- First, we must check that $M$ is a type, i.e., a term in *some* universe.
Because there is only one universe we denote it by $\mathcal{U}$. The formation
rule for types has no computation content, then the usage is zero in this case.
- Second, the term $x$ needs to be checked against $M$ using the same usage
$\sigma$ we need in the conclusion. The context for this is $\Gamma$. There is
one issue here. This type checking expects $M$ to be in normal form. When it is
not, typechecking the judgment $\Gamma \vdash x \Leftarrow^\sigma M$ may give us
a false negative.

  - *Example*: Why do we need $M'$? Imagine that we want to infer the type of $v$ given $\Gamma \vdash x : \mathsf{Ann}(v, \mathsf{Vec}(\mathsf{Nat},2+2))$. Clearly, the answer should be `Vec(Nat,4)`. 
  However, this reasoning step requires computation. $$\Gamma \vdash x : \mathsf{Ann}(v, \mathsf{Vec}(\mathsf{Nat},2+2)) \Rightarrow \mathsf{Vec}(\mathsf{Nat},4))\,.$$

- Using $M'$ as the normal form of $M$, it remains to check if $x$ is of type
$M'$. If so, the returning type is $M'$ and the resources map has to be updated
(the $\color{gray}{gray}$ $\Theta$ in the rule below).

$$
\begin{gathered}
\rule{Ann{⇒}}{
0\cdot \Gamma \vdash M \Leftarrow^0 \mathcal{U}
\qquad
M \rightsquigarrow M'
\qquad
\Gamma \vdash x \Leftarrow^\sigma M' \color{darkgrey}{\dashv \Theta}
}{
  \Gamma \vdash \mathsf{Ann}(x,M) \Rightarrow^{\sigma} M'
    \color{darkgrey}{\dashv \Theta}
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

## Application

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
\sigma\pi\cdot\Gamma' \vdash x : ^{\sigma\pi} A
}{
\Gamma + \sigma\pi\cdot\Gamma'  \vdash f\,x :^{\sigma} B
}
\end{gathered}
%
$$

The first judgement about $f$ is *principal*. Then, it must be an inference step.
After having inferred the type of $f$, the types $A$ and $B$ become known facts.
It is then time to check the type of $x$ against $A$. 

$$\begin{gathered}
\rule{App{\Rightarrow_2}}{
\Gamma \vdash f {\color{blue}\Rightarrow}^{\sigma}(x : ^\pi A) \to  B
\qquad
\sigma\pi\cdot\Gamma' \vdash x {\color{red}\Leftarrow}^{\sigma\pi} A
}{
\Gamma + \sigma\pi\cdot\Gamma'  \vdash f\,x\,{\color{blue}\Rightarrow^{\sigma}}\, B
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
0\cdot \Gamma \vdash x {\color{red}\Leftarrow^{0}} A
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

## Tensor type elimination

In QTT, there is no $\Sigma$-types but instead tensor product types.
As with any other elimination rule, we synthetise the type in the principal judgement,
which it is the first premise. In our case, the first premise is the fact that $M$ is a tensor product type. Then, the types $A$ and $B$ are known facts.

TODO: use notebook page

$$\begin{gathered}
\rule{TensorElim{\Rightarrow}}{
\Gamma_{1} \vdash M \,\overset{\sigma}{\color{blue}\Rightarrow}\,(x\overset{\pi}{:}A)\otimes B 
\\
0\cdot \Gamma_{1},z\overset{0}{:}(x\overset{\pi}{:}A)\otimes B \vdash C \,\overset{0}{\color{red}\Leftarrow}\,\mathcal{U}
\qquad
\Gamma_{2},x\overset{\sigma\pi}{:}A, y\overset{\sigma}{:}B\vdash N \overset{\sigma}{\color{red}{\Leftarrow}}C[(x,y)/z]\qquad
}{
\Gamma_{1}+\Gamma_{2} \vdash \mathrm{let}\,z@(x,y)=M\,\,\mathrm{in}\,\,N :C \overset{\sigma}{\color{blue}\Rightarrow}\, C[M/x] 
}
\end{gathered}
%
$$

TODO: I need the syntax "let" as we need pattern-matching to make sure one uses all the components in the pair. Exactly the same situation occurs in linear logic, which actually inspires this rule.

## Sum type elim

TODO



## Typing rules summary

TODO put all the typing rules without bidirectional syntax (intro, elims, computation rules).