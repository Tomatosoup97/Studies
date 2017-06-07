    data Request = Readline
                 | PutStrLn String

    data Response = Success
                  | Str String

    type Dialog = [Response] -> [Request]


    execRequest :: Request -> IO Response
    execRequest Readline = getLine >>= \s -> return (Str s)
    execRequest (PutStrLn s) = putStrLn s >> return Success


    dialogToIOMonad :: Dialog -> IO ()
    dialogToIOMonad dialog =
        let getFirstReq :: Dialog -> Request
            getFirstReq dialog = let (req:_) = dialog [] in req

            getTailReqs :: Dialog -> Response -> Dialog
            getTailReqs dialog resp =
                \resps -> let (_:reqs) = dialog (resp:resps) in reqs
        in do
            let req = getFirstReq dialog
            resp <- execRequest req
            dialogToIOMonad (getTailReqs dialog resp)
