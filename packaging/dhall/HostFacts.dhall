-- HostFacts.dhall — Stable host identity and hardware facts for honey
--
-- Narrow host-facts surface for Dhall consumers that need Dell-T7810-specific
-- identity and topology without importing the whole boot-generation lane.
--
-- Keep live boot entries, UUIDs, storage layout, and deployment state in
-- defaults/*.dhall. Keep workstation evidence and host-contract docs in the
-- companion Dell-7810 repo.

let NUMANode =
      { cpus : Text, ramMiB : Natural, distanceSelf : Natural, distanceCross : Natural }

let FirmwareInventory =
      { total : Natural, dxeDrivers : Natural, smmHandlers : Natural, peiModules : Natural }

let HostFacts =
      { name : Text
      , vendor : Text
      , model : Text
      , boardId : Text
      , biosVersion : Text
      , biosSha256 : Text
      , ramGiB : Natural
      , gpu : Text
      , firmwareModules : FirmwareInventory
      , numa : List NUMANode
      }

let honey
    : HostFacts
    = { name = "honey"
      , vendor = "Dell"
      , model = "Precision Tower 7810"
      , boardId = "0GWHMW"
      , biosVersion = "A34"
      , biosSha256 =
          "6a1c9a01683453881c610c5771fb225a024b1b2122da0cf6f95a43e870a77ff9"
      , ramGiB = 220
      , gpu = "AMD Radeon RX 9070 XT (Navi 48 / RDNA4)"
      , firmwareModules =
          { total = 497, dxeDrivers = 270, smmHandlers = 153, peiModules = 72 }
      , numa =
          [ { cpus = "0-7,16-23", ramMiB = 131709, distanceSelf = 10, distanceCross = 21 }
          , { cpus = "8-15,24-31", ramMiB = 98514, distanceSelf = 10, distanceCross = 21 }
          ]
      }

in  { NUMANode, FirmwareInventory, HostFacts, honey }
