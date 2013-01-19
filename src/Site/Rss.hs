module Site.Rss where

------------------------------------------------------------------------------
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------

-- | Inserts link to site rss
-- <link rel="alternate" type="application/rss+xml" title="Лента" href="/rss"/>
rssSplice :: Splice AppHandler