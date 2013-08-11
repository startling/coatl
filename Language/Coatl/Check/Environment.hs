module Language.Coatl.Check.Environment where
import Control.Monad.Error
-- containers
import Data.Map (Map)
import qualified Data.Map as M
-- mtl
import Control.Monad.Reader
-- lens
import Control.Lens
-- coatl
import Language.Coatl.Parse.Syntax
import Language.Coatl.Abstract
import Language.Coatl.Evaluate

data Environment a v = Environment
  { _named       :: APrism' v Canonical
    -- ^ A prism into the 'Canonical' in the symbol type.
  , _types       :: Map v (Value v)
    -- ^ The types of things that have already been checked
    --   in the environment. They should be in normal form.
  , _definitions :: Map v (Value v)
    -- ^ The values already defined.
  }
makeLenses ''Environment

-- | Run a checking action in an environment with something new
--   as 'Nothing'.
with :: Ord v
  => Value v
  -> ReaderT (Environment a (Maybe v)) m b
  -> ReaderT (Environment a v) m b
with a = withReaderT (set (types . at Nothing)
  (Just . fmap Just $ a) . lower) where
    lower :: Ord v => Environment a v -> Environment a (Maybe v)
    lower (Environment n ts ds) = Environment (_Just . n)
      (M.mapKeys Just . M.map (fmap Just) $ ts)
      (M.mapKeys Just . M.map (fmap Just) $ ds)
