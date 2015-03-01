module Main where

import Control.Monad.Eff
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler


wowHandler :: Handler
wowHandler = send "WOW"

appSetup :: App
appSetup = do
    get "/wow" wowHandler

attach :: Application -> ExpressM Unit
attach = apply appSetup

-- Pretend this is some external JS file and we want
-- to attach our PS handlers into express app that
-- already exists there.
foreign import realMain
    """
    function realMain(attachFn) {
        return function() {
            var app = require('express')();
            app.get('/', function(req, res) {
                res.send('Hello!');
            })
            attachFn(app)();
            app.listen(8080);
        }
    }
    """ :: forall e. (Application -> ExpressM Unit) -> Eff e Unit


main = realMain attach

