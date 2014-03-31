This package allows you to automatically derive hxt picklers
(conversions to and from xml) using the regular generics package.

A simple example:

```Haskell
{-# LANGUAGE TemplateHaskell
           , EmptyDataDecls
           , TypeFamilies
           #-}

import Generics.Regular (deriveAll, PF)
import Text.XML.HXT.Arrow.Pickle (XmlPickler (..))
import Generics.Regular.XmlPickler (gxpickle)

data User = User
  { name  :: String
  , admin :: Bool
  }

-- Derive Regular instance.
deriveAll ''User "PFUser"
type instance PF User = PFUser

-- Define generic pickler instance.
instance XmlPickler User where
  xpickle = gxpickle
```

Now you can use the functionality from `Text.XML.HXT.Arrow.Pickle`.
For example:

```
> showPickled [] (User "Simon" True)

"<user><name>Simon</name><admin>true</admin></user>"

> unpickleDoc xpickle $ head $ xread "<user><name>Simon</name><admin>true</admin></user>" :: Maybe User

Just (User {name = "Simon", admin = True})
```
