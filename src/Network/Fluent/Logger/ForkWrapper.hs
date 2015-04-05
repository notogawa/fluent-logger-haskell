{-# LANGUAGE CPP #-}
module Network.Fluent.Logger.ForkWrapper (forkIOUnmasked) where

import qualified Control.Concurrent as CC
#if MIN_VERSION_base(4,3,0)
#else
import qualified Control.Exception as CE
#endif

forkIOUnmasked :: IO () -> IO CC.ThreadId
#if MIN_VERSION_base(4,4,0)
forkIOUnmasked action = CC.forkIOWithUnmask (\unmask -> unmask action)
#elif MIN_VERSION_base(4,3,0)
forkIOUnmasked action = CC.forkIOUnmasked
#else
forkIOUnmasked action = if CE.blocked then CE.unblock $ CC.forkIO action else CC.forkIO action
#endif

