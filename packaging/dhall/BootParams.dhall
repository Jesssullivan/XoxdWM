-- BootParams.dhall — Generate kernel boot parameters from platform definition
--
-- Composes reusable boot-parameter fragments from a narrower host-timing
-- surface plus workload requirements.

let Platform = (./Platform.dhall).Platform
let HostTiming = ./HostTiming.dhall

let Workload =
      { name : Text
      , isolatedCores : Text
      , housekeepingCores : Text
      , requireRT : Bool
      , requireTSC : Bool
      , idlePoll : Bool
      }

let bciWorkload
    : Workload
    = { name = "BCI/VR"
      , isolatedCores = HostTiming.workloadDefaults.isolatedCores
      , housekeepingCores = HostTiming.workloadDefaults.housekeepingCores
      , requireRT = True
      , requireTSC = True
      , idlePoll = HostTiming.workloadDefaults.idlePoll
      }

let smiMitigationText =
      \(platform : Platform) -> HostTiming.smiMitigationText platform.cpu.tscReliable

let workloadText =
      \(w : Workload) ->
        HostTiming.workloadIsolationText w.isolatedCores w.housekeepingCores w.idlePoll

let runtimeText =
      \(w : Workload) -> HostTiming.runtimeIsolationText w.housekeepingCores

let hardwareText = HostTiming.gpuDisplayText

let debugText = HostTiming.debugText

let allText =
      \(platform : Platform) ->
        \(workload : Workload) ->
          "${smiMitigationText platform} ${workloadText workload} ${hardwareText}"

let xrText =
      \(platform : Platform) ->
        \(workload : Workload) ->
          "${allText platform workload} ${runtimeText workload}"

in  { Workload
    , bciWorkload
    , smiMitigationText
    , workloadText
    , runtimeText
    , hardwareText
    , debugText
    , allText
    , xrText
    }
