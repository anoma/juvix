# Example

A Juvix Markdown file name ends with `.juvix.md`. This kind of file must contain
a module declaration at the top, as shown below ---in the first code block.

<pre class="highlight"><code class="juvix"><pre class="src-content"><span class="ju-keyword">module</span> <span id="Y0"><span class="annot"><a href="X#Y0"><span class="annot"><a href="X#Y0"><span class="ju-var">Test</span></a></span></a></span></span><span class="ju-delimiter">;</span><br/></pre></code></pre>

Certain blocks can be hidden from the output by adding the `hide` attribute, as shown below.



<pre class="highlight"><code class="juvix"><pre class="src-content"><span id="Y748"><span class="annot"><a href="X#Y748"><span class="annot"><a href="X#Y748"><span class="ju-function">fib</span></a></span></a></span></span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">→</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">→</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">→</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span><br/>  <span class="ju-keyword">|</span> <span class="annot"><a href="X#Y124"><span class="ju-constructor">zero</span></a></span> <span id="Y751"><span class="annot"><a href="X#Y751"><span class="annot"><a href="X#Y751"><span class="ju-var">x1</span></a></span></a></span></span> <span class="ju-keyword">_</span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#Y751"><span class="ju-var">x1</span></a></span><br/>  <span class="ju-keyword">|</span> <span class="annot"><a href="X#Y125"><span class="ju-constructor"><span class="ju-delimiter">(</span>suc</span></a></span> <span id="Y752"><span class="annot"><a href="X#Y752"><span class="annot"><a href="X#Y752"><span class="ju-var">n</span></a></span></a></span></span><span class="ju-delimiter">)</span> <span id="Y753"><span class="annot"><a href="X#Y753"><span class="annot"><a href="X#Y753"><span class="ju-var">x1</span></a></span></a></span></span> <span id="Y754"><span class="annot"><a href="X#Y754"><span class="annot"><a href="X#Y754"><span class="ju-var">x2</span></a></span></a></span></span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#Y748"><span class="ju-function">fib</span></a></span> <span class="annot"><a href="X#Y752"><span class="ju-var">n</span></a></span> <span class="annot"><a href="X#Y754"><span class="ju-var">x2</span></a></span> <span class="annot"><a href="X#Y753"><span class="ju-var"><span class="ju-delimiter">(</span>x1</span></a></span> <span class="annot"><a href="X#Y517"><span class="ju-function">+</span></a></span> <span class="annot"><a href="X#Y754"><span class="ju-var">x2</span></a></span><span class="ju-delimiter">)</span><span class="ju-delimiter">;</span><br/><br/><span id="Y749"><span class="annot"><a href="X#Y749"><span class="annot"><a href="X#Y749"><span class="ju-function">fibonacci</span></a></span></a></span></span> <span class="ju-delimiter">(</span><span class="annot"><a href="X#Y755"><span class="ju-var">n</span></a></span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span><span class="ju-delimiter">)</span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#Y748"><span class="ju-function">fib</span></a></span> <span class="annot"><a href="X#Y755"><span class="ju-var">n</span></a></span> <span class="ju-number">0</span> <span class="ju-number">1</span><span class="ju-delimiter">;</span></pre></code></pre>

Commands like `typecheck` and `compile` can be used with Juvix Markdown files.

<pre class="highlight"><code class="juvix"><pre class="src-content"><span id="Y750"><span class="annot"><a href="X#Y750"><span class="annot"><a href="X#Y750"><span class="ju-function">main</span></a></span></a></span></span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#Y722"><span class="ju-axiom">IO</span></a></span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#Y727"><span class="ju-axiom">readLn</span></a></span> <span class="annot"><a href="X#Y735"><span class="ju-function"><span class="ju-delimiter">(</span>printNatLn</span></a></span> <span class="annot"><a href="X#Y200"><span class="ju-function">∘</span></a></span> <span class="annot"><a href="X#Y749"><span class="ju-function">fibonacci</span></a></span> <span class="annot"><a href="X#Y200"><span class="ju-function">∘</span></a></span> <span class="annot"><a href="X#Y566"><span class="ju-axiom">stringToNat</span></a></span><span class="ju-delimiter">)</span><span class="ju-delimiter">;</span></pre></code></pre>

Other code blocks are not touched, e.g:

```text
This is a text block
```


```haskell
module Test where
```

Blocks indented.

  ```haskell
    module Test where
  ```

Empty blocks:

```
```

We also use other markup for documentation such as:

!!! note

    We use this kind of markup for notes, solutions, and other stuff

    1. More text

        ```text
        f {n : Nat := 0} {m : Nat := n + 1} ....
        ```

    2. Second text


??? info "Solution"

    Initial function arguments that match variables or wildcards in all clauses can
    be moved to the left of the colon in the function definition. For example,

    <pre class="highlight"><code class="juvix"><pre class="src-content"><span class="ju-keyword">module</span> <span id="Y759"><span class="annot"><a href="X#Y759"><span class="annot"><a href="X#Y759"><span class="ju-var">move-to-left</span></a></span></a></span></span><span class="ju-delimiter">;</span><br/>  <span class="ju-keyword">import</span> <span class="annot"><a href="X#Y512"><span class="ju-var">Stdlib<span class="ju-delimiter">.</span>Data<span class="ju-delimiter">.</span>Nat</span></a></span> <span class="ju-keyword">open</span><span class="ju-delimiter">;</span><br/>  <span id="Y756"><span class="annot"><a href="X#Y756"><span class="annot"><a href="X#Y756"><span class="ju-function"><br/>  add</span></a></span></a></span></span> <span class="ju-delimiter">(</span><span class="annot"><a href="X#Y757"><span class="ju-var">n</span></a></span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span><span class="ju-delimiter">)</span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">-&gt;</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span><br/>    <span class="ju-keyword">|</span> <span class="annot"><a href="X#Y124"><span class="ju-constructor">zero</span></a></span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#Y757"><span class="ju-var">n</span></a></span><br/>    <span class="ju-keyword">|</span> <span class="annot"><a href="X#Y125"><span class="ju-constructor"><span class="ju-delimiter">(</span>suc</span></a></span> <span id="Y758"><span class="annot"><a href="X#Y758"><span class="annot"><a href="X#Y758"><span class="ju-var">m</span></a></span></a></span></span><span class="ju-delimiter">)</span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#Y125"><span class="ju-constructor">suc</span></a></span> <span class="annot"><a href="X#Y756"><span class="ju-function"><span class="ju-delimiter">(</span>add</span></a></span> <span class="annot"><a href="X#Y757"><span class="ju-var">n</span></a></span> <span class="annot"><a href="X#Y758"><span class="ju-var">m</span></a></span><span class="ju-delimiter">)</span><span class="ju-delimiter">;</span><br/><span class="ju-keyword">end</span><span class="ju-delimiter">;</span></pre></code></pre>

    is equivalent to

    <pre class="highlight"><code class="juvix"><pre class="src-content"><span class="ju-keyword">module</span> <span id="Y764"><span class="annot"><a href="X#Y764"><span class="annot"><a href="X#Y764"><span class="ju-var">example-add</span></a></span></a></span></span><span class="ju-delimiter">;</span><br/>  <span class="ju-keyword">import</span> <span class="annot"><a href="X#Y512"><span class="ju-var">Stdlib<span class="ju-delimiter">.</span>Data<span class="ju-delimiter">.</span>Nat</span></a></span> <span class="ju-keyword">open</span><span class="ju-delimiter">;</span><br/>  <span id="Y760"><span class="annot"><a href="X#Y760"><span class="annot"><a href="X#Y760"><span class="ju-function"><br/>  add</span></a></span></a></span></span> <span class="ju-keyword">:</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">-&gt;</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span> <span class="ju-keyword">-&gt;</span> <span class="annot"><a href="X#Y123"><span class="ju-inductive">Nat</span></a></span><br/>    <span class="ju-keyword">|</span> <span id="Y761"><span class="annot"><a href="X#Y761"><span class="annot"><a href="X#Y761"><span class="ju-var">n</span></a></span></a></span></span> <span class="annot"><a href="X#Y124"><span class="ju-constructor">zero</span></a></span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#Y761"><span class="ju-var">n</span></a></span><br/>    <span class="ju-keyword">|</span> <span id="Y762"><span class="annot"><a href="X#Y762"><span class="annot"><a href="X#Y762"><span class="ju-var">n</span></a></span></a></span></span> <span class="annot"><a href="X#Y125"><span class="ju-constructor"><span class="ju-delimiter">(</span>suc</span></a></span> <span id="Y763"><span class="annot"><a href="X#Y763"><span class="annot"><a href="X#Y763"><span class="ju-var">m</span></a></span></a></span></span><span class="ju-delimiter">)</span> <span class="ju-keyword">:=</span> <span class="annot"><a href="X#Y125"><span class="ju-constructor">suc</span></a></span> <span class="annot"><a href="X#Y760"><span class="ju-function"><span class="ju-delimiter">(</span>add</span></a></span> <span class="annot"><a href="X#Y762"><span class="ju-var">n</span></a></span> <span class="annot"><a href="X#Y763"><span class="ju-var">m</span></a></span><span class="ju-delimiter">)</span><span class="ju-delimiter">;</span><br/><span class="ju-keyword">end</span><span class="ju-delimiter">;</span></pre></code></pre>
