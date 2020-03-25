{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -XDeriveDataTypeable #-}
-- | Core components of  Includes definitions for typeclasses, data types, the Anomy monad and whatever else that
-- might be go in here later on.
module Core.Types (
      -- * Message wrappers
        Inc, Out, Ignore
      , inc, out, ignore
      -- * Type classes
      , Auth (..)
      , Incoming (..)
      , Outgoing (..)
      -- * The Anomy monad
      , Anomy
      , evalAnomy
      , execAnomy
      , io, ask, asks
      , get, gets, put, modify
      , AConf (..)
      , AState (..)
      , AException (..)
) where

import Auis.Reader.Types as AR (Auis)
import Auis.Writer.Types as AW (Auis)
import Bncs.Reader.Types as BR (Bncs)
import Bncs.Writer.Types as BW (Bncs)
import Core.Common.Types (Dword, Flag, Rank, Member)

import Control.Concurrent.STM (TChan)
import Control.Exception (Exception)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask, asks)
import Control.Monad.State (StateT, MonadState, evalStateT, execStateT, get, gets, put, modify)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map (Map)
import Data.Typeable (Typeable)
import Network.Socket (HostName, Socket)
import System.IO (Handle)


-- | Existential quantification for the Incoming class.
data Inc = forall i. Incoming i => Inc i
-- data Inc = 


-- | Existential quantification for the Outgoing class.
data Out = forall o. Outgoing o => Out o


-- | Ignored 'Outgoing' message.
data Ignore = Ignore


-- | No one needs to know what Inc looks like on the inside.
inc :: Incoming i => i -> Inc
inc = Inc


-- | No one needs to know what Out looks like on the inside.
out :: Outgoing o => o -> Out
out = Out


-- | Ignore me please. A do-nothing message.
ignore :: Ignore
ignore = Ignore


-- | Authorisation class.
class Auth u where
      isAuthorised :: u -> Bool


-- | Incoming message typeclass. Used to separate incoming from outgoing messages and hide the underlying types.
class Incoming i where
      respMsg :: i -> Anomy [Out]
      respMsg _ = return $! []


-- | Class instance for 'Inc'.
instance Incoming Inc where
      respMsg (Inc m) = respMsg m


-- | Outgoing message typeclass. Used to separate incoming from outgoing messages and hide the underlying types.
class Outgoing o where
      execMsg :: o -> Anomy ()
      execMsg _ = return $! ()


-- | Class instance for 'Out'.
instance Outgoing Out where
      execMsg (Out m) = execMsg m


-- | Instantiate 'Ignore' here.
instance Outgoing Ignore


-- | The Anomy monad. Essentially identical in form to XMonad. Based on the X monad. The Anomy monad is an IO-based
-- State and Reader monad-transformed container which facilitates the management of Anomy's mutable state and read-
-- only configuration.
newtype Anomy a = Anomy (ReaderT AConf (StateT AState IO) a)
      deriving (Monad, MonadIO, MonadReader AConf, MonadState AState)


-- | Run the 'Anomy' monad with the give 'Anomy' code /a/ using  configuration /aconf/ and state /astate/. Discards the
-- end state.
evalAnomy :: AConf -> AState -> Anomy a -> IO a
evalAnomy aconf astate (Anomy a) = evalStateT (runReaderT a aconf) astate


-- | Run the 'Anomy' monad with the give 'Anomy' code /a/ using  configuration /aconf/ and state /astate/. Returns the
-- end state.
execAnomy :: AConf -> AState -> Anomy a -> IO AState
execAnomy aconf astate (Anomy a) = execStateT (runReaderT a aconf) astate


-- | For convenience.
io :: IO a -> Anomy a
io = liftIO


-- | Configuration record contains the read-only variables changeable only outside runtime. Monospaced text in the
-- comments below show how a particular option in the configuration file would appear.
--
-- /Note: all options must be present and valid in the configuration file and all options and option values are CASE SENSITIVE./
data AConf = AConf
      { -- | PvPGN hostname.
        --
        -- @ netHostname = \"pvpgn.boredaussie.com\" @
        netHostname           :: !HostName
        -- | PvPGN client port.
        --
        -- @ netPort = 6112 @
      , netPort               :: !Int
        -- | PvPGN game (hosting) port.
        --
        -- @ netGamePort = 6113 @
      , netGamePort           :: !Int
        -- | Wait time (in seconds) before retrying a connect.
        -- Only waits if a connection was not successful in the
        -- previous session.
        --
        -- @ netRCWait = 3 @
      , netRCWait             :: !Int

        -- | Account username.
        --
        -- @ accUsername = \"Anomy\" @
      , accUsername           :: !String
        -- | Account password.
        --
        -- @ accPassword = \"iAm1337\" @
      , accPassword           :: !String
        -- | Account email. Note: only required for creating an account.
        --
        -- @ accEmail = \"anomy\@gmail.com\" @
        --
        -- Recommended:
        --
        -- @ accEmail = \"\" @
      , accEmail              :: !String
        -- | Channel to join upon logon.
        --
        -- Using 'Nothing' causes the PvPGN server to assign you a default channel.
        --
        -- @ accChannel = Nothing @
        --
        -- Using 'Just' allows you to join a custom channel.
        --
        -- @ accChannel = Just \"rated\" @
      , accChannel            :: !(Maybe String)

        -- | Auto-logon once connected.
        --
        -- @ autoLogon = True @
      , autoLogon             :: !Bool
        -- | Auto-accept clan invitations (if possible).
        --
        -- @ autoAccept = False @
      , autoAccept            :: !Bool
      }
      deriving (Read, Show)


-- | State record contains the mutable variables which govern and are governed by runtime operation.
data AState = AState
      { -- | Network socket.
        netSock               :: !Socket

        -- | Outgoing AUIS message queue.
      , chanAuis              :: !(TChan AW.Auis)
        -- | Outgoing BNCS message queue.
      , chanBncs              :: !(TChan BW.Bncs)
        -- | Incoming AUIS and BNCS message queue.
      , chanCore              :: !(TChan Inc)
      
        -- | Handle to log file
      , logHandle             :: !Handle
        -- | String of log filename
      , logFileName           :: !String

        -- | Self user flag.
      , selfFlag              :: !Flag
        -- | Self clan tag.
      , selfTag               :: !(Maybe Dword)
        -- | Self clan rank.
      , selfRank              :: !(Maybe Rank)
        -- | Self currently in channel.
      , selfChannel           :: !(Maybe String)

        -- | Internal list of users in channel.
      , sizeChannel           :: !Int
        -- | Internal list of friends.
      , sizeFriends           :: Int
        -- | Internal list of clan members.
      , sizeClan              :: !Int -- count online members
      , mapClan               :: !(Map String Member)

        -- | Clan tag for creation.
      , createTag             :: !(Maybe Dword)
        -- | Clan name for creation.
      , createClan            :: !(Maybe String)

        -- | Clan invitiation (reply) cookie dword.
      , inviteCookie          :: !(Maybe Dword)
        -- | Clan invitation (reply) tag.
      , inviteTag             :: !(Maybe Dword)
        -- | Clan invitation (reply) name.
      , inviteClan            :: !(Maybe String)
        -- | Clan invitation (reply) sender.
      , inviteSender          :: !(Maybe String)

        -- | Clan invitation (send) invitee.
      , inviteUsername        :: !(Maybe String)
        -- | Username of member to be removed.
      , removeUsername        :: !(Maybe String)
        -- | Chieftain transfer variable.
      , transChieftain        :: !(Maybe String)

        -- | Rank change username (send).
      , changeUsername        :: !(Maybe String)
        -- | Rank change to (send).
      , changeRank            :: !(Maybe Rank)
      }


-- | Exception types specific to 'Anomy'
data AException
      = AStop
      | ARestart
      deriving (Show, Typeable)

instance Exception AException