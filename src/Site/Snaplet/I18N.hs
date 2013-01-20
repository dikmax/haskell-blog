{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies,
    MultiParamTypeClasses #-}
module Site.Snaplet.I18N where

------------------------------------------------------------------------------
import           Data.Text (Text)
import           Snap
import           Snap.Snaplet
import           Text.Shakespeare.I18N
------------------------------------------------------------------------------

data I18N = I18N Text

class HasI18N b where
  i18nLens :: SnapletLens b I18N

data BlogApp = BlogApp

mkMessage "BlogApp" "snaplets/i18n" "ru"

i18nInit :: SnapletInit b I18N
i18nInit = makeSnaplet "i18n" "I18N snaplet" Nothing $ do
  return $ I18N "ru"

setLanguage :: HasI18N b => Text -> Handler b b ()
setLanguage = with i18nLens . Snap.put . I18N

translate :: HasI18N b => BlogAppMessage -> Handler b b Text
translate message = with i18nLens $ do
  I18N lang <- Snap.get
  return $ renderMessage BlogApp [lang] message

