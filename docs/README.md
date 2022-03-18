# Proposal : MiniJuvix [draft]

[![hackmd-github-sync-badge](https://hackmd.io/R8f7FXyOTAKdQ5_cZ-L-og/badge)](https://hackmd.io/R8f7FXyOTAKdQ5_cZ-L-og)

<!-- Latex stuff adapted from Andy's record proposal: 
https://hackmd.io/Xlvu82eFQ_m-cebUWAtGsw?edit -->

$$
% ---------- To typeset grammars -----------------------------
\renewcommand\.{\mathord.}
\newcommand{\EQ}{\mkern5mu\mathrel{::=}}
\newcommand{\OR}[1][]{\mkern17mu | \mkern12mu}
\newcommand{\Or}{\mathrel|}
\newcommand{\RT}[1]{\{#1\}}
\newcommand{\RV}[1]{\langle#1\rangle}
\newcommand{\Let}{\mathbf{let}\:}
\newcommand{\Q}{\mathrel|}
\newcommand{\I}{\color{blue}}
\newcommand{\O}{\color{green}}
% ---------- To typese rules ---------------------------------
\newcommand{\rule}[3]{%
\frac{%
\begin{gathered}\,{#2}%
\end{gathered}%
}{#3}\:{\mathsf{#1}}}
% Bidirectional judgements
\newcommand{\check}[4]{{{#1}\,\vdash\,{#2}\,\overset{{#3}}{\color{red}{\Leftarrow}}\, {#4}}}
\newcommand{\infer}[4]{{{#1}\,\vdash\,{#2}\,\overset{{#3}}{\color{blue}{\Rightarrow}}\, {#4}}}
% ------------------------------------------------------------
$$

###### tags: `Juvix`

## Abstract 

MiniJuvix implements a programming language that takes variable resources very
seriously in the programs. As mathematical foundation, we are inspired by
Quantitative type theory (QTT), a dependent type theory that marriages ideas
from linear logic and traditional dependently typed programs to writing memory-efficient programs using resource accounting. Among the language features, there is a type for a universe,
dependent function types, tensor products, sum types, and more type formers.

The main
purpose of MiniJuvix is to serve as a guide to supporting/extending the
[Juvix](/Q5LbuHI5RXaJ8mD08yW7-g) programming language, in particular, the design
of a correct and efficient typechecker.

In this document we provide a work in progress report containing a description
of the MiniJuvix bidirectional type checking algorithm. We have provide some
Haskell sketches for the algorithm implementation.

The code will be available on the Github repository:
[heliaxdev/MiniJuvix](https://github.com/heliaxdev/MiniJuvix). In this document,
we only refer to the implementation provided in the `qtt` branch.

## Language

### Syntax

**Quantities** In the traditional QTT, each term has a usage/quantity annotation
in the semiring $\{0,1,\omega\}$. Besides the semiring structure, we also
consider different ordering of these terms. One choice for such an order states
that $0,1 < \omega$ and $0$ and $1$ are not comparable, i.e., $0 \not < 1$. This
order is good, at least in the sense that terms of zero usage and $1$ usage live
in completely different worlds, from the evaluation point-of-view. Terms or zero
usage are *irrelevant* at runtime, and we therefore erase them in the *erasure*
phase. While, non-zero terms are *present* during compilation and execution of
the program. We embrace this distinction in the implementation with the data type
`Relevant` with constructors *irrelevant* and *relevant*.


$$
\begin{aligned}
%x,y,z &\EQ \dotsb & \text{term variable} \\[.5em]
\pi,\rho,\sigma &\EQ 0 \Or 1 \Or \omega
  & \text{(quantity variables)} \\[.5em]
\end{aligned}
$$
<!-- A, B &\EQ \mathcal{U}                  & \text{universe} \\
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
\Delta &\EQ () \Or (x : A)\,\Delta& \text{ telescopes} -->

**Judgements** The core language in MiniJuvix is bidirectional syntax-directed,
meaning that a *judgement* in the theory contains a term that is either
*checkable* or *inferable*. A type judgement consists of a *context*, a *term* --the term quantity required--, a *judgement mode*, and a *type*. Precisely, the judgement
mode is either *checking* or *inferring*, as illustrated in the rules below,
respectively, using a red and blue arrow.

$$
\begin{gathered}
\check{\Gamma}{t}{\sigma}{M}  
\qquad
\infer{\Gamma}{t}{\sigma}{M}
\end{gathered}
%
$$

We will refer to the *erased* part of the theory when the variable resource
$\sigma$ is zero in the type judgement ; otherwise, we are in the *present*
segment of the theory. Another way to refer to this distinction is that no
computation is required in the $\sigma$ zero theory. Otherwise, we say that the judgement possess *computation content*.


**Contexts** The context in which a term is judged is fundamental to determine
well-formed terms. Another name for context is *environment*. A context can be
either empty or it can be extended by binding name annotations of the form $x
\overset{\sigma}{:} M$ for a given type $A$. 

$$
\begin{aligned}
\Gamma &\EQ \emptyset \Or (\Gamma, x\overset{\pi}{:} A) & \text{(contexts)}
\end{aligned}
$$

A needed context operation is *scaling*. The *scalar product* with a context
$\Gamma$ is defined by induction on the context structure. Given a scalar number
$\sigma$, the product $\sigma \cdot \Gamma$ denotes the context $\Gamma$ after
multiplying *all* its resources variables by $\sigma$.

$$
\begin{aligned}
\color{green}{\sigma} \cdot \emptyset &:= \emptyset,\\
\color{green}{\sigma} \cdot (\Gamma, x \overset{\color{green}{\pi}}{:} A) &:= \color{green}{\sigma} \cdot \Gamma , x \overset{\color{green}{\sigma\cdot \pi}}{:} A.
\end{aligned}
$$

The addition operation for contexts is a binary operation only defined between
contexts with the same variable set. The latter condition is the proposition
stating $0 \cdot \Gamma_1 = 0 \cdot \Gamma_2$ between contexts $\Gamma_1$ and
$\Gamma_2$. Consequently, adding contexts create another context with the same
variables from the input but with their resource summed up. 

$$
\begin{aligned}
\emptyset+\emptyset &:=\emptyset \\
(\Gamma_{1}, x \overset{\color{green}{\sigma}}{:} A)+(\Gamma_{2}, x \overset{\color{green}{\pi}}{:} A) &:=(\Gamma_{1}+\Gamma_{2}), x^{\color{green}{\sigma+\pi}} S
\end{aligned}
$$

**Telescopes** A *resource* telescope is the name for grouping types with
resource information. We use telescopes in forming new types, for
example, in forming new inductive types.

\begin{aligned}
\Delta &\EQ () \Or \Delta(x \overset{\sigma}{:} A) & \text{(telescopes)}
\end{aligned}

The $\color{gray}{gray}$ cases below are expected to be incorporated in the future.

**Types** A *type* in the theory is one of the following synthactical cases.  

$$
\begin{aligned}
A , B%
&\EQ \mathcal{U} & \text{(universe type)} \\
&\OR (x \overset{\sigma}{:} A) \to B       &\text{(depend. fun. type)} \\
&\OR (x \overset{\sigma}{:} A) \otimes B   &\text{(tensor prod. type)} \\
&\OR A + B   &\text{(sum type)} \\
&\OR 1   &\text{(unit type)} \\
&\OR \color{gray}{P} &\color{gray}{\text{(primitive type)}}\\
&\OR \color{gray}{D} &\color{gray}{\text{(inductive type decl.)}}\\
&\OR \color{gray}{R} &\color{gray}{\text{(record type decl.)}}
\end{aligned}
$$

On the other hand, we want to consider a set of *primitive types*, each of these with a set of *primitive* terms. An example of a primitive types is that of the type
of *boolens*, denoted by $\mathsf{Bool}$.  $\mathsf{true} : \mathsf{Bool}$ and
$\mathsf{False} : \mathsf{Bool}$.

**Terms** We refer to terms as those elements that can inhabit a type. So far,
we have used as a term the metavariable $x$. A term can take one of the
following shapes. 

$$
\begin{aligned}
u, v , t , f &\EQ \mathsf{Var}(x)    &\text{(variable)}\\
&\OR \mathsf{Ann}(x,A)           &\text{(type annotation)}\\
&\OR \mathsf{Lam}(x,t)           &\text{(lambda abstraction)}\\
&\OR\mathsf{App}(u,v)            &\text{(application)}\\
&\OR *                           &\text{(unit)}\\
&\OR \color{gray}{\mathsf{Fun}} &\color{gray}{\text{(named function)}}\\
&\OR \color{gray}{\mathsf{Con}} &\color{gray}{\text{(data constr.)}} 
\end{aligned}
$$

The explicit naming below like $\mathsf{Ann}$ is on purpose. We want to avoid
any confussion, for example, between type annotations and usage type annotation,
i.e., $x : A$ and $x \overset{\sigma}{:} A$.



# Typing rules

We present the types rules almost in the same way as one would expect to see them in the
implementation, i.e., using the bidirectional notation. 

It must be assumed that contexts appearing in the rules are *well-formed*, i.e. terms build up using the following derivation rules. 

$$
\rule{\mathsf{empty}\mbox{-}\mathsf{ctx}}{
%
}
{\emptyset \ \mathsf{ctx}}
\qquad
\rule{,\mbox{-}\mathsf{ctx}}{
\Gamma \ \mathsf{ctx}
\qquad
\color{green}{\Gamma \vdash A \ \mathsf{type}}
\qquad
\sigma \ \mathsf{Quantity}
}
{
(\Gamma , x \overset{\sigma}{:} A) \ \mathsf{ctx}
}
$$
$$
\rule{\cdot\mbox{-}\mathsf{ctx}}{%
\Gamma \ \mathsf{ctx} \qquad \sigma \ \mathsf{Quantity}
}{
\sigma \cdot \Gamma \ \mathsf{ctx}
}\qquad
\rule{\mbox{+}\mbox{-}\mathsf{ctx}}{%
\Gamma_1 \ \mathsf{ctx} \qquad \Gamma_2 \ \mathsf{ctx} \qquad  0\cdot \Gamma_1 = 0 \cdot \Gamma_2
}{
\Gamma_1 + \Gamma_2 \ \mathsf{ctx}
}
$$

Below, we describe the algorithms for checking and infering types in a mutually
defined way. The corresponding algorithms are the functions `check` and `infer`
in the implementation. The definition of each is the collection and
interpretation of the typing rules (reading them bottom to top).

For example, the `infer` method is defined to work with three
arguments: one implicit argument for the context $\Gamma$ and two explicit
arguments, respectively, the term $t$ and its quantity $\sigma$. The output of the algorithm is precisely the type $M$ for $t$ in the rule below.

$$
\begin{gathered}
\rule{}{
p_1 \cdots\ p_n
}{
\infer{\Gamma}{t}{\sigma}{M}
}
\end{gathered}
%
$$

The variables $p_i$ in the rule above are inner steps of the algorithm and the order
in which they are presented matters. For example, an inner step can be infering
a type, checking if a property holds for a term, reducing a term, or simply
checking a term against another type. 

A *reduction* step is denoted by $\Gamma \vdash
t \rightsquigarrow t'$ or simply by $t \rightsquigarrow t'$ whenever the context
$\Gamma$ is known. Such a reduction is obtained by calling `eval` in the
implementation.

**Remark.**  A term in the [Core](https://github.com/heliaxdev/MiniJuvix/blob/qtt/src/MiniJuvix/Syntax/Core.agda) implementation is either a Checkable or Inferable term. We refer to these
options as the *mode* of the term. In a typing rule the strategy/mode in a type
judgement determines the mode of the term in the conclusion. In the example above, $t$ is Inferable.

```haskell
data Term : Set where
  Checkable : CheckableTerm → Term  -- terms with a type checkable.
  Inferable : InferableTerm → Term  -- terms that which types can be inferred.
```


**TODO**: we need to reflect on how we introduce the judgement $\Gamma \vdash A\  \mathsf{type}$ of the well-formed types. This may change the way of presenting formation rules, as the first ones below.

## Checking rules

This section mainly refers to the construction of checkable terms in the
implementation.

*Remark.* We omit comments in the formation rules below. The general idea is
that no resources are needed to form a type. Therefore, we only check when
forming a type in the erase part of the theory, for both, premises and
conclusion.


- [x] UniverseType
- [x] PiType
- [x] Lam
- [x] TensorType
- [x] TensorIntro
- [x] UnitType
- [x] Unit 
- [ ] SumType



### Universe

**Formation rule**

$$
\rule{}{\Gamma \ \mathsf{ctx} }{\Gamma \vdash \mathcal{U} \ \mathsf{type}}
\qquad
\begin{gathered}
\rule{Univ{\Leftarrow}}{
(0\cdot \Gamma) \ \mathsf{ctx} 
}{
0\cdot \Gamma \vdash \mathcal{U}\, \overset{0}{\color{red}\Leftarrow}\,\, \mathcal{U}
}
\end{gathered}
%
$$

### Dependent function types

**Formation rule**

$$
\rule{}{\Gamma \ \mathsf{ctx}
\qquad \Gamma \vdash A \ \mathsf{type}
\qquad (\Gamma , x \overset{\sigma}{:} A) \vdash B(x) \ \mathsf{type}
}{\Gamma \vdash (x \overset{\sigma}{:} A) \to B \ \ \mathsf{type}}
\\
\begin{gathered}
\rule{Pi{\Leftarrow}}{
0\cdot \Gamma \vdash A \, \overset{0}{\color{red}\Leftarrow}\,\mathcal{U}
\qquad
(0\cdot \Gamma,\,x\overset{0}{:}A)\vdash B \, \overset{0}{\color{red}\Leftarrow}\,\mathcal{U}\quad
}{
0\cdot \Gamma \vdash (x\overset{\pi}{:}A)\rightarrow B \overset{0}{\color{red}\Leftarrow}\mathcal{U}
}
\end{gathered}
%
$$

**Introduction rule**

The lambda abstraction rule is the introduction rule of a dependent function
type. The principal judgement in this case is the conclusion, and we therefore
check against the corresponing type $(x \overset{\sigma}{:} A) \to B$. With
the types $A$ and $B$, all the information about the premise
becomes known, and it just remains to check its judgement.



$$\begin{gathered}
\rule{Lam{\Leftarrow}}{
(\Gamma, x\overset{\sigma\pi}{:}A) \vdash b \,\overset{\sigma}{\color{red}\Leftarrow}\,B
}{
\Gamma \vdash \lambda x.b \overset{\sigma}{\color{red}\Leftarrow} (x\overset{\pi}{:}A)\rightarrow B
}
\end{gathered}
%
$$

Another choice here is $\lambda x.b$ instead of $\lambda\,\mathsf{Ann}(x,A). b$.
The former option will change the strategy, to infer the conclusion, since one would have enough information for typing.

### Tensor product types

**Formation rule**

$$\begin{gathered}
\rule{\otimes\mbox{-}{\Leftarrow}}{
0\cdot \Gamma \vdash A \,\overset{0}{\color{red}\Leftarrow}\,\mathcal{U}
\qquad
(0\cdot \Gamma, x\overset{0}{:}A) \vdash B\overset{0}{\color{red}\Leftarrow}\,\mathcal{U}\quad
}{
0\cdot \Gamma \vdash (x\overset{\pi}{:}A) \otimes B \overset{0}{\color{red}\Leftarrow}\,\mathcal{U}
}
\end{gathered}
%
$$

**Introduction rule**

A rule to introduce pairs in QTT appears in [Section 2.1.3 in Atkey's
paper](https://bentnib.org/quantitative-type-theory.pdf). We here present this rule
in a more didactical way but also following the bidirectional
recipe. Briefly, the known rule is splitted in two cases, the erased and present
part of the theory, after studying the usage variable in the conclusion. Recall
that forming pairs is the way one introduces values of the tensor product.
One then must check the rule conclusion. After doing this, the types $A$ and $B$ become
known facts and it makes sense to check the types in the premises. The usage
bussiness follows a similar reasoning as infering applications. 


$$\begin{gathered}
\rule{\otimes I{\Leftarrow}}{
\check{\sigma\pi \cdot \Gamma_1}{u}{\sigma\pi}{A}
\qquad
\check{\Gamma_2}{v}{\sigma}{B[u/x]}\quad
\color{gray}{0 \cdot \Gamma_1 = 0\cdot \Gamma_2}\quad
}{
\check{\sigma\pi\cdot \Gamma_1 + \Gamma_2}{(u,v)}{\sigma}{(x\overset{\pi}{:}A) \otimes B}
}
\end{gathered}
%
$$

Essentially, we are
forming $\sigma$ *dependent* pairs where the first cordinate, $u$, is used $\pi$
times in the second component. This is the reason for having $\sigma\pi\cdot
\Gamma_1$ in the conclusion since,  $u$ is taken from $\Gamma_1$. The gray
premises below are necessary, since one must ensure that the addition between
context is possible.

Finally, we obtain the following two rules that make up the original one.


1. $$\begin{gathered}
\rule{\otimes I_1{\Leftarrow}}{
\color{green}{\sigma\pi = 0}
\qquad
0\cdot \Gamma \vdash u \,\overset{0}{\color{red}\Leftarrow}\,A
\qquad
\Gamma \vdash v \,\overset{\sigma}{\color{red}\Leftarrow}\,B[u/x]
\qquad 
}{
\Gamma \vdash (u,v)\overset{\sigma}{\color{red}\Leftarrow} (x\overset{\pi}{:}A) \otimes B
}
\end{gathered}
%
$$

2. $$\begin{gathered}
\rule{\otimes I_2{\Leftarrow}}{
\color{green}{\sigma\pi \neq 0}\qquad
\Gamma_{1} \vdash u \,\overset{1}{\color{red}\Leftarrow}\,A
\qquad
\Gamma_{2} \vdash v \,\overset{\sigma}{\color{red}\Leftarrow}\,B[u/x]
\qquad
\color{gray}{0 \cdot \Gamma_1 = 0\cdot \Gamma_2}\quad
}{
\color{green}{\sigma\pi}\cdot \Gamma_{1}+\Gamma_{2} \vdash (u,v)\overset{\sigma}{\color{red}\Leftarrow} (x\overset{\pi}{:}A) \otimes B 
}
\end{gathered}
%
$$

### Unit type

$$
\rule{}{
0 \cdot \Gamma \ \mathsf{ctx}
}{
\Gamma \vdash 1 \ \mathsf{type}
}
\qquad
\rule{1\mbox{-}I}{
0 \cdot \Gamma \ \mathsf{ctx}
}{
\check{0\cdot\Gamma}{1}{0}{\mathcal{U}}
}
\qquad 
\rule{*\mbox{-}I}{
0 \cdot \Gamma \ \mathsf{ctx}
}{
\check{0\cdot\Gamma}{*}{0}{1}
}
$$

### Sum type

TODO

### Inductive types

TODO


## Conversion rules


Include the rules for definitional equality:
- [ ] β-equality, 
- [ ] reflexivity, 
- [ ] symmetry, 
- [ ] transitivity, and
- [ ] congruence.


$$\begin{gathered}
\rule{conv{\Leftarrow}}{
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


## Type inference

The algorithm that implements type inference is called `infer`. Inspired by Agda and its inference strategy, MiniJuvix only infer values that are *uniquely* determined by the context.
There are no guesses. Either we fail or get a unique answer, giving us a predicatable behaviour. 


By design, a term is inferable if it is one of the following cases.

- [x] Variable
- [x] Annotation
- [x] Application
- [x] Tensor type elim
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

### Variable

A variable can be *free* or *bound*. If the variable is free, the rule is as
follows.

**Free variable**

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

**Bound variables**

The case of the`Bound` variable throws an error.

### Annotations

$$
\begin{gathered}
\rule{Ann{⇒}}{
0\cdot \Gamma \vdash A\,{\color{red}\Leftarrow}^0\,\mathcal{U}
\qquad
\Gamma \vdash x\,{\color{red}\Leftarrow}^\sigma\, A
}{
  \Gamma \vdash \mathsf{Ann}(x,A)\,{\color{blue}\Rightarrow}^{\sigma}\,A
}
\end{gathered}
%
$$

Any annotation possess type information that counts as known facts, and we therefore infer. However, this is a choice.

- First, we must check that $A$ is a type, i.e., a term in *some* universe.
Because there is only one universe we denote it by $\mathcal{U}$. The formation
rule for types has no computation content, then the usage is zero in this case.
- Second, the term $x$ needs to be checked against $A$ using the same usage
$\sigma$ we need in the conclusion. The context for this is $\Gamma$. There is
one issue here. This type checking expects $A$ to be in normal form. When it is
not, typechecking the judgement $\Gamma \vdash x \Leftarrow^\sigma A$ may give us
a false negative.

  - *Example*: Why do we need $A'$? Imagine that we want to infer the type of $v$ given $\Gamma \vdash x : \mathsf{Ann}(v, \mathsf{Vec}(\mathsf{Nat},2+2))$. Clearly, the answer should be `Vec(Nat,4)`. 
  However, this reasoning step requires computation. $$\Gamma \vdash x : \mathsf{Ann}(v, \mathsf{Vec}(\mathsf{Nat},2+2)) \Rightarrow \mathsf{Vec}(\mathsf{Nat},4))\,.$$

- Using $M'$ as the normal form of $A$, it remains to check if $x$ is of type
$A'$. If so, the returning type is $A'$ and the table resources has to be updated
(the $\color{gray}{gray}$ $\Theta$ in the rule below).

$$
\begin{gathered}
\rule{Ann{⇒}}{
\check{0\cdot \Gamma}{A}{0}{\mathcal{U}}
\qquad
A \color{green}{\rightsquigarrow} A'
\qquad
\check{\Gamma}{x}{\sigma}{A'} \color{darkgrey}{\dashv \Theta}
}{
\infer{\Gamma}{\mathsf{Ann}(x,A)}{\sigma}{A'}
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

### Application

**Elimination rule**

Recall the task is to find $A$ in $\Gamma \vdash \mathsf{App}(f,x) :^{\sigma}
A$. If we follow the bidirectional type-checking recipe, then it makes sense to
infer the type for an application, i.e., $\Gamma \vdash \mathsf{App}(f,x)
\Rightarrow^{\sigma} A$. An application essentially removes a lambda abstraction
introduced earlier in the derivation tree. The rule for this inference case is a
bit more settle, especially because of the usage variables.

To introduce the term of an application, $\mathsf{App}(f,x)$, it requires to
give/have a judgement saying that $f$ is a (dependent) function, i.e., $\Gamma
\vdash f \overset{\sigma}{:} (x \overset{\pi}{:} A) \to  B$, for usages variables $\sigma$ and
$\pi$. Then, given $\Gamma$, the function $f$ uses $\pi$ times its input,
mandatory. We therefore need $\sigma\pi$ resources of an input for $f$ if we
want to apply $f$ $\sigma$ times, as in the conclusion $\Gamma \vdash
\mathsf{App}(f,x) \Rightarrow^{\sigma} A$.

In summary, the elimination rule is often presented as follows.

$$\begin{gathered}
\rule{}{
\Gamma \vdash f :^{\sigma} (x : ^\pi A) \to  B
\qquad
\sigma\pi\cdot\Gamma' \vdash x : ^{\sigma\pi} A
}{
\Gamma + \sigma\pi\cdot\Gamma'  \vdash \mathsf{App}(f,x)  :^{\sigma} B
}
\end{gathered}
%
$$

The first judgement about $f$ is *principal*. Then, it must be an inference step.
After having inferred the type of $f$, the types $A$ and $B$ become known facts.
It is then correct to check the type of $x$ against $A$. 

$$\begin{gathered}
\rule{}{
\Gamma \vdash f {\color{blue}\Rightarrow}^{\sigma}(x : ^\pi A) \to  B
\qquad
\sigma\pi\cdot\Gamma' \vdash x {\color{red}\Leftarrow}^{\sigma\pi} A
\qquad
\color{gray}{0 \cdot \Gamma = 0 \cdot \Gamma'}
}{
\Gamma + \sigma\pi\cdot\Gamma'  \vdash \mathsf{App}(f,x) \,{\color{blue}\Rightarrow^{\sigma}}\, B
}
\end{gathered}
%
$$

To make our life much easier, the rule above can be splitted in two cases,
emphasising the usage bussiness.

1. $$\begin{gathered}
\rule{App{\Rightarrow_1}}{
\color{green}{\sigma \cdot \pi = 0}
\qquad
\Gamma \vdash f {\color{blue}\Rightarrow^{\sigma}} (x :^{\pi} A) \to B
\qquad
0\cdot \Gamma \vdash x {\color{red}\Leftarrow^{0}} A
\qquad
}{
\infer{\Gamma}{\mathsf{App}(f,x)}{\sigma}{B}
}
\end{gathered}
%
$$

2. $$\begin{gathered}
\rule{App{\Rightarrow_2}}{
\color{green}{\sigma \cdot \pi \neq 0}
\qquad
\infer{\Gamma_1}{f}{\sigma}{(x :^{\pi} A) \to B}
\qquad
\check{\Gamma_2}{x}{1}{A}
\qquad
\color{gray}{0 \cdot \Gamma_1 = 0 \cdot \Gamma_2}
\quad
}{
\infer{\Gamma_1 + \sigma \pi\cdot \Gamma_2}{\mathsf{App}(f,x)}{\sigma}{B}
}
\end{gathered}
$$


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

In the rules above, we have the lemma:

- $1 \cdot \Gamma \vdash x :^1 A$ entails that $\sigma \cdot \Gamma \vdash x
:^\sigma A$ for any usage $\sigma$.


## Tensor products

**Elimination rule**

In Atkey's QTT, there is no $\Sigma$-types but instead tensor product types. As
with any other elimination rule, in the principal judgement, we synthetise a
type. In our case, the principal judgement shows up in the first premise, which
is the fact that $M$ is a tensor product type. If we infer that, the types $A$
and $B$ become known facts. Then, the type $C$, depending on $A$ and $B$ become checkable, also making the next judgement checking.



$$\begin{gathered}
\rule{TensorElim{\Rightarrow}}{
\infer{\Gamma_{1}}{M}{\sigma}{(x\overset{\pi}{:}A)\otimes B}
\\
\check{(0\cdot \Gamma_{1},z\overset{0}{:}(x\overset{\pi}{:}A)\otimes B)}{C}{0}{\mathcal{U}}
\\
\check{(\Gamma_{2}, u \overset{\sigma\pi}{:} A, v\overset{\sigma}{:}B)}{%
N}{\sigma}{C[(x,y)/z]}
}{
\Gamma_{1}+\Gamma_{2} \vdash \mathsf{let}\,z@(u,v)=M\,\,\mathsf{in}\,\,N :C \overset{\sigma}{\color{blue}\Rightarrow}\, C[M/x] 
}
\end{gathered}
%
$$

*Remark* Inspired by the tensor product rules in linear logic, there is a need to
decompose a pair in its components. We have to be sure that all the resources in
each component are effectively used. This mechanism needs to be introduced somewhere somehow, Idk yet. It is the keyword $\mathsf{let}\mbox{-}\mathsf{in}$.

## Sum type elim

TODO


## References

- Robert Atkey. 2018. Syntax and Semantics of Quantitative Type Theory. In Proceedings of the 33rd Annual ACM/IEEE Symposium on Logic in Computer Science (LICS '18). Association for Computing Machinery, New York, NY, USA, 56–65. DOI:https://doi.org/10.1145/3209108.3209189
- Jana Dunfield and Neel Krishnaswami. 2021. Bidirectional Typing. ACM Comput. Surv. 54, 5, Article 98 (June 2022), 38 pages. DOI:https://doi.org/10.1145/3450952
- James Wood, Robert Atkey. A Linear Algebra Approach to Linear Metatheory. Arxiv: https://arxiv.org/abs/2005.02247
- Andy Morris. Juvix: Core language documentation. https://juvix.readthedocs.io/en/latest/compiler/core/core-language.html#id6
- Andy Morris. Proposal: Records in Core. https://hackmd.io/UT269VN1R6-qchHaWzg7KQ
- SVOBODA, Tomáš. Additive Pairs in Quantitative Type Theory. Praha, 2021. Diplomová práce Univerzita Karlova, Matematicko-fyzikální fakulta, Katedra algebry. Vedoucí práce Šefl, Vít. http://hdl.handle.net/20.500.11956/127263
- Conor McBride. 2016. I Got Plenty o' Nuttin'. In A List of Successes That Can Change the World-Essays Dedicated to Philip Wadler on the Occasion of His $60 t h$ Birthday. 207-233. DOI:https://doi.org/10.1007/978-3-319-30936-1
