module MidMonad
where
import Control.Monad.MRWS
import StackTypes (Settings, AppState)
import LLM.OpenAI (Usage)

-- monad that handles all application's business logic
type Mid = MRWST Settings Usage AppState IO
