module Mockdata exposing (..)

import Route exposing (..)
import Dict exposing (..)

hosts : List Host
hosts =
    [ { id = "HostId1"
      , name = "Host 1"
      }
    , { id = "HostId2"
      , name = "Host 2"
      }
    , { id = "HostId3"
      , name = "Host 3"
      }
    ]

appointments : Dict HostId (List Appointment)
appointments = fromList
    [ ("HostId1", [ Appointment "2018-12-24 08:00" 30
                   , Appointment "2018-12-24 09:00" 30
                   , Appointment "2018-12-24 10:00" 30
                   , Appointment "2018-12-24 11:00" 30
                   , Appointment "2018-12-24 13:00" 30
                   , Appointment "2018-12-24 14:00" 30
                   , Appointment "2018-12-24 15:00" 30
                   , Appointment "2018-12-24 16:00" 30
                   ])
    , ("HostId2", [ Appointment "2018-12-24 08:00" 30
                   , Appointment "2018-12-24 09:00" 30
                   , Appointment "2018-12-24 10:00" 30
                   , Appointment "2018-12-24 11:00" 30
                   , Appointment "2018-12-24 13:00" 30
                   , Appointment "2018-12-24 14:00" 30
                   , Appointment "2018-12-24 15:00" 30
                   , Appointment "2018-12-24 16:00" 30
                   ])
    , ("HostId3", [ Appointment "2018-12-24 08:00" 30
                   , Appointment "2018-12-24 09:00" 30
                   , Appointment "2018-12-24 10:00" 30
                   , Appointment "2018-12-24 11:00" 30
                   , Appointment "2018-12-24 13:00" 30
                   , Appointment "2018-12-24 14:00" 30
                   , Appointment "2018-12-24 15:00" 30
                   , Appointment "2018-12-24 16:00" 30
                   ])
    ]