module Test where
    open import Agda.Primitive
    postulate B : Set lzero
    postulate b : B 

    data A ( f : Set lzero -> Set lzero -> Set lzero) : Set lzero where
        magic : A f -> f B (A f) -> A f