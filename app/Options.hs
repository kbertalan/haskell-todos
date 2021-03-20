module Options where

import App
import App.DB as DB
import App.Ekg as Ekg
import App.Random as Random
import App.Web as Web

defaultAppOptions :: App.Options
defaultAppOptions =
  App.Options
    { web =
        Web.Options
          { webPort = 3000
          },
      db =
        DB.Options
          { poolSize = 10,
            poolTimeout = 5,
            dbHost = "localhost",
            dbPort = 5432,
            dbUser = "postgres",
            dbPassword = "postgres",
            dbName = "postgres"
          },
      ekg =
        Ekg.Options
          { host = "localhost",
            port = 8000
          },
      random =
        Random.Options
          { seed = Random.New
          }
    }
