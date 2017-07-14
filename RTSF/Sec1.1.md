type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens s t a b = forall f. Functor f => s -> (a -> f b) -> f t
type Lens s t a b = s -> forall f. Functor f => (a -> f b) -> f t
type Lens s t a b = s -> (a, b -> t)
type Lens s t a b = (s -> a, s -> b -> t)
s -> a is a getter
s -> b -> t is a setter

-------------------------------------------------------------------

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal s t a b = forall f. Applicative f => s -> (a -> f b) -> f t
type Traversal s t a b = s -> forall f. Applicative f => (a -> f b) -> f t
type Traversal s t a b = s -> ([a], [b] -> t)
type Traversal s t a b = (s -> [a], s -> [b] -> t)