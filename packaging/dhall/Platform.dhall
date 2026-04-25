-- Platform.dhall — Type-safe hardware platform definitions
--
-- Defines the Dell T7810 (Grantley/Wellsburg) platform characteristics
-- discovered via firmware RE of BIOS A34. Used to generate:
--   - Kernel config fragments (scripts/config calls)
--   - Boot parameters (grubby / BLS entries)
--   - tuned profiles
--   - SMI validation thresholds

let HostFacts = ./HostFacts.dhall

let SMISource =
      { name : Text
      , smiEnBit : Natural
      , biosDisable : Optional Text
      , kernelParam : Optional Text
      , risk : Text
      , description : Text
      }

let PCH =
      { name : Text
      , codename : Text
      , acpiBase : Natural
      , smiEnPort : Natural
      , smiStsPort : Natural
      , smiSources : List SMISource
      }

let CPU =
      { name : Text
      , codename : Text
      , sockets : Natural
      , coresPerSocket : Natural
      , threadsPerCore : Natural
      , tscReliable : Bool
      , tscDeadlineErrata : Bool
      , microcodeFixBios : Text
      }

let NUMANode = HostFacts.NUMANode

let SMIBaseline =
      { worstCaseUs : Natural, periodicRateHz : Double, totalFromBoot : Natural, biosVersion : Text }

let Platform =
      { name : Text
      , vendor : Text
      , model : Text
      , boardId : Text
      , biosVersion : Text
      , biosSha256 : Text
      , pch : PCH
      , cpu : CPU
      , ramGiB : Natural
      , gpu : Text
      , firmwareModules :
          { total : Natural
          , dxeDrivers : Natural
          , smmHandlers : Natural
          , peiModules : Natural
          }
      , numa : List NUMANode
      , smiBaseline : SMIBaseline
      }

let hostFacts = HostFacts.honey

let wellsburg
    : PCH
    = { name = "Intel C610/C612"
      , codename = "Wellsburg"
      , acpiBase = 0x0400
      , smiEnPort = 0x0430
      , smiStsPort = 0x0434
      , smiSources =
        [ { name = "USB Legacy 1.1"
          , smiEnBit = 3
          , biosDisable = Some "Disable USB Legacy Support"
          , kernelParam = None Text
          , risk = "CRITICAL"
          , description = "EHCI legacy emulation — 24 refs in PCH dispatcher"
          }
        , { name = "APMC (Software SMI)"
          , smiEnBit = 5
          , biosDisable = None Text
          , kernelParam = None Text
          , risk = "HIGH"
          , description = "Software SMI port — used by Dell SMBIOS DA/CI"
          }
        , { name = "TCO Watchdog"
          , smiEnBit = 13
          , biosDisable = None Text
          , kernelParam = None Text
          , risk = "MEDIUM"
          , description = "TCO watchdog timer — disable via CONFIG_TCO_WATCHDOG=n"
          }
        , { name = "Periodic Timer"
          , smiEnBit = 14
          , biosDisable = None Text
          , kernelParam = None Text
          , risk = "MEDIUM"
          , description = "Periodic SMI timer — Dell Smart Timer uses this"
          }
        , { name = "USB Legacy 2.0"
          , smiEnBit = 17
          , biosDisable = Some "Disable USB Legacy Support"
          , kernelParam = None Text
          , risk = "CRITICAL"
          , description = "xHCI legacy emulation — 33 refs in PCH dispatcher"
          }
        ]
      }

let haswellEP
    : CPU
    = { name = "Xeon E5-2630 v3"
      , codename = "Haswell-EP"
      , sockets = 2
      , coresPerSocket = 8
      , threadsPerCore = 2
      , tscReliable = True
      , tscDeadlineErrata = True
      , microcodeFixBios = "A34"
      }

let dellT7810
    : Platform
    = { name = hostFacts.name
      , vendor = hostFacts.vendor
      , model = hostFacts.model
      , boardId = hostFacts.boardId
      , biosVersion = hostFacts.biosVersion
      , biosSha256 = hostFacts.biosSha256
      , pch = wellsburg
      , cpu = haswellEP
      , ramGiB = hostFacts.ramGiB
      , gpu = hostFacts.gpu
      , firmwareModules = hostFacts.firmwareModules
      , numa = hostFacts.numa
      -- SMI baseline measured on BIOS A02; honey now runs A34 (TSC errata fixed)
      , smiBaseline =
          { worstCaseUs = 2523
          , periodicRateHz = 1.0
          , totalFromBoot = 9959
          , biosVersion = "A02"
          }
      }

in  { Platform, PCH, CPU, SMISource, NUMANode, SMIBaseline, dellT7810, wellsburg, haswellEP }
