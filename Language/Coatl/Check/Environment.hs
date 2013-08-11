module Language.Coatl.Check.Environment where
-- containers
import Data.Map (Map)
import qualified Data.Map as M
-- mtl
import Control.Monad.Reader
-- lens
import Control.Lens
-- coatl
import Language.Coatl.Syntax
import Language.Coatl.Abstract
import Language.Coatl.Evaluate

data Environment a v = Environment
  { _types       :: Map v (Value v)
    -- ^ The types of things that have already been checked
    --   in the environment. They should be in normal form.
  , _definitions :: Map v (Value v)
    -- ^ The values already defined.
  }
makeLenses ''Environment

data Checking a v = Checking
  { _named       :: APrism' v Canonical
    -- ^ A prism into the 'Canonical' in the symbol type.
  , _environment :: Environment a v
    -- ^ The environment.
  }
makeLenses ''Checking

-- | Run a checking action in an environment with something new
--   as 'Nothing'.
with :: Ord v
  => Value v
  -> ReaderT (Checking a (Maybe v)) m b
  -> ReaderT (Checking a v) m b
with a = withReaderT (set (environment . types . at Nothing)
  (Just . fmap Just $ a) . lower) where
    lower :: Ord v => Checking a v -> Checking a (Maybe v)
    lower (Checking n (Environment ts ds)) = Checking (_Just . n)
      $ Environment
        (M.mapKeys Just . M.map (fmap Just) $ ts)
        (M.mapKeys Just . M.map (fmap Just) $ ds)
