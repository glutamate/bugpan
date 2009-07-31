module QueryPluginAPI where 

import QueryTypes

data  Interface = Interface {
      theQuery :: String-> IO QueryResultBox
}

plugin = Interface { theQuery = \_ -> return $ QResBox ([]::[Event Double]) }

