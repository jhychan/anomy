-- | Initialises 'Anomy' with a beginning state and configuration and forks threads to make 'Anomy' function as a whole.
-- Includes a default/new state and configuration to begin with.
module Anomy where

import Auis as A
import Bncs as B
import Core as C

import Control.Monad (when)
import Prelude hiding (catch)


-- | Initialise the GTK+ interface. Draws up Anomy, spawns the writer thread and adds signal handlers.
main :: IO ()
main = do
      -- Core and UI state initialisation.
      astate <- initCore
      istate <- initGui
      
      -- Load configuration from file at startup to style GUI and populate GUI config fields. This
      -- is the only time configuration is read from file.
      aconf <- loadConf astate newAConf fileAConf
      iconf <- loadConf astate newIConf fileIConf
      initConf istate iconf aconf
      
      -- install signal handlers and pass in main handler loop with reusable state.
      initSignals istate iconf astate aconf
            -- function to start the core of 'Anomy'
            (spawn (startCore istate astate aconf))
            -- funtion to stop the core of 'Anomy'
            (stopCore)
      
      -- Thread the UI writer to 'enable' the UI. Allows processing of 'Auis' messages to occur and
      -- functions to be queued for the executing when the main GUI loop finally starts.
      _ <- spawn $ A.writer istate (chanAuis astate)
      
      -- go go go!
      mainGUI



-- Overloaded function to read configurations from file with exception handling.
loadConf :: (Show a, Read a) => AState -> a -> String -> IO a
loadConf astate newconf fileconf = let
            writeAuis = writeTChanIO (chanAuis astate)
            conftype =
                  case fileconf == fileAConf of
                        True -> "core configuration file."
                        False -> "interface configuration file."
      in do
      catch
            ( do
                  writeAuis (specialMsg ("Reading " ++ conftype))
                  conf <- readConf fileconf newconf
                  writeAuis (specialMsg ("Successfully read " ++ conftype))
                  return conf
            )
            ( -- only runs if failed to read config.
              \excepread -> do
                  -- notify of failure immediately
                  writeAuis (errorMsg ("Failed to read " ++ conftype))
                  writeAuis (exceptionMsg excepread)
                  -- if missing file, write the default config.
                  when (isDoesNotExistError excepread)
                        ( catch
                              ( do
                                    writeAuis (specialMsg ("Writing new " ++ conftype))
                                    writeConf fileconf newconf
                                    writeAuis (specialMsg ("Successfully wrote new " ++ conftype))
                              )
                              ( -- only runs if failed to write config.
                                \excepwrite -> do
                                    writeAuis (errorMsg ("Failed to write new " ++ conftype))
                                    writeAuis (exceptionMsg (excepwrite :: IOException))
                              )
                        )
                  -- return the default config.
                  return $! newconf
            )


-- | Loop entrance for the core handler of 'Anomy'
startCore :: IState -> AState -> AConf -> IO ()
startCore istate astate aconf_ = do
      -- get a new socket...
      netsock <- newSocket
      -- then refresh the configuration...
      aconf <- getAConf istate aconf_
      -- before running a new session.
      runCore istate (astate { netSock = netsock }) aconf


-- | This is a new session. Refreshes itself upon any errors or disconnects.
runCore :: IState -> AState -> AConf -> IO ()
runCore istate astate aconf = let
            netsock  = netSock astate
            chanauis = chanAuis astate
            chanbncs = chanBncs astate
            chancore = chanCore astate
            writeAuis = writeTChanIO chanauis
            -- writeBncs = writeTChanIO chanbncs
            writeCore = writeTChanIO chancore

            runCoreStart :: IO ()
            runCoreStart = do
                  writeAuis (specialMsg "Starting a new session...")

            runCoreConnect :: IO ()
            runCoreConnect = catch
                  ( do  -- attempt to connect.
                        writeAuis (internalMsg ("Connecting to \"" ++ netHostname aconf ++ "\" on port " ++ shows (netPort aconf) "..."))
                        connectSocket netsock (netHostname aconf) (netPort aconf)

                        writeAuis (internalMsg "Successfully connected to server.")
                        -- tell PvPGN we're connecting as game client rather than chat client.
                        sendMsg netsock "\x01"
                  )
                  ( -- only runs if failed to connect or stop called.
                    \excep -> do
                        writeAuis (errorMsg "Failed to connect to server.")
                        writeAuis (exceptionMsg (excep :: IOException))
                        -- wait a little before trying to reconnect.
                        threadDelay (1000000 * abs (netRCWait aconf))
                        -- refresh the state and run a new 'core' \'session\'.
                        startCore istate astate aconf
                  )

            runCoreHandler :: IO ()
            runCoreHandler = catch
                  ( -- treat bncs reader and writer thread IDs as releasable resource.
                    bracket
                        ( do  -- thread bncs reader and writer. return for killing in event of disconnect.
                              -- writeAuis (errorMsg "Starting bncs reader...")
                              rtid <- spawn (B.reader netsock chancore)
                              -- writeAuis (errorMsg "Starting bncs writer...")
                              wtid <- spawn (B.writer netsock chanbncs)
                              -- writeAuis (errorMsg "Bncs threads should be started...")
                              return $! (rtid, wtid)
                        )
                        ( \(rtid, wtid) -> do
                              -- guarantees that reader and writer threads will be killed if an internal error occurs.
                              -- writeAuis (errorMsg "Killing bncs writer...")
                              killThread wtid
                              -- writeAuis (errorMsg "Killing bncs reader...")
                              -- FIXED: start/stop failure on Windows
                              -- killThread fails due to the FFI call in recvLen/recvFrom blocking the thread.
                              -- spawn the killing thread in another thread and close the socket to unblock recvLen.
                              spawn $ killThread rtid
                              -- writeAuis (errorMsg "Bncs threads should be killed.")
                        )
                        ( \(_, _) -> do
                              -- send the auto-logon check message.
                              writeCore (inc autologon)
                              -- run the core handler loop.
                              evalAnomy aconf astate handler
                        )
                  )
                  ( -- only runs if reader, writer or handler raises an error, or 'AException' thrown.
                    \excep -> do
                        -- socket is likely disconnected, or should be disconnected.
                        closeSocket (netSock astate)
                        -- 'Anomy' is UBER STABLE. SURELY it was a disconnection. ;P
                        writeAuis (errorMsg "Disconnected from server.")
                        -- Display error if it is something other than an 'Anomy' specific exception.
                        when (not (isAException excep)) (writeAuis (exceptionMsg excep))
                        -- Run new sesions if exception is not a Stop.
                        when (not (isAStop excep)) (startCore istate astate aconf)
                  )

      in do
            -- notify user of new session.
            runCoreStart
            -- connection sequence with exceptions.
            runCoreConnect
            -- we're connected so start the remaining core loops to get a fully working session.
            runCoreHandler



-- | Permanently ends an 'Anomy' session.
stopCore :: ThreadId -> IO ()
stopCore corethread = do
      throwTo corethread aeAStop
