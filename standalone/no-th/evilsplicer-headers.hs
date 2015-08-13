

{- This file was modified by the EvilSplicer, adding these headers,
 - and expanding Template Haskell. 
 -
 - ** DO NOT COMMIT ** 
 -}
import qualified Data.Monoid
import qualified Data.Set
import qualified Data.Set as Data.Set.Base
import qualified Data.Map
import qualified Data.Map as Data.Map.Base
import qualified Data.HashMap.Strict
import qualified Data.HashMap.Strict as Data.HashMap.Base
import qualified Data.Foldable
import qualified Data.Text
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder as Data.Text.Internal.Builder
import qualified Text.Shakespeare
import qualified Text.Hamlet
import qualified Text.Julius
import qualified Text.Css
import qualified "blaze-markup" Text.Blaze.Internal
import qualified Yesod.Core.Widget
import qualified Yesod.Routes.TH.Types
import qualified Yesod.Core.Dispatch
import qualified Yesod.Routes.Dispatch
import qualified WaiAppStatic.Storage.Embedded
import qualified WaiAppStatic.Storage.Embedded.Runtime
import qualified Data.FileEmbed
import qualified Data.ByteString.Internal
import qualified Data.Text.Encoding
import qualified Network.Wai
import qualified Network.Wai as Network.Wai.Internal
import qualified Yesod.Core.Types
import qualified GHC.IO
import qualified Data.ByteString.Unsafe
import qualified Data.ByteString.Char8
import qualified Database.Persist.Class as Database.Persist.Class.PersistField
import qualified Database.Persist as Database.Persist.Class.PersistField
import qualified Database.Persist.Sql as Database.Persist.Sql.Class
import qualified Database.Persist.Sql as Database.Persist.Types.Base
import qualified Control.Monad.Logger
import qualified Control.Monad.IO.Class
import qualified Control.Monad.Trans.Control
import Database.Persist.Sql (fromPersistValue)
{- End EvilSplicer headers. -}


