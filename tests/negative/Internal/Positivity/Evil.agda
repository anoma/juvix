module Evil where
    
    open import Agda.Primitive
    postulate B : Set lzero
    postulate b : B
    g : Set lzero -> Set lzero
    g  x = x
    
    data A (f : Set lzero -> Set lzero) : Set lzero where
        magic : f (A g) -> A f 