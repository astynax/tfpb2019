module These where

data These a b
  = This a
  | That b
  | These a b
  | Nope

instance (Semigroup a, Semigroup b) => Semigroup (These a b) where
  This a      <> This b      = This (a <> b)
  That a      <> That b      = That (a <> b)
  These a1 b1 <> These a2 b2 = These (a1 <> a2) (b1 <> b2)
  This a      <> That b      = These a b
  That a      <> This b      = These b a
  This a1     <> These a2 b2 = These (a1 <> a2) b2
  That b1     <> These a2 b2 = These a2 (b1 <> b2)
  These a1 b1 <> This a2     = These (a1 <> a2) b1
  These a1 b1 <> That b2     = These a1 (b1 <> b2)
  Nope <> x = x
  x <> Nope = x

instance (Monoid a, Monoid b) => Monoid (These a b) where
  mempty = Nope
