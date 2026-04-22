-- HostTiming.dhall — Stable Dell T7810 host timing posture fragments
--
-- This is a narrow Dhall surface for host timing and isolation posture.
-- It exists so boot generations stop hardcoding Dell timing facts inline.
--
-- It is still an operational consumer-side copy inside XoxdWM. The underlying
-- workstation evidence and host-contract authority live in the companion
-- Dell-7810 repo.

let baseSMIMitigationText =
        "tsc=nowatchdog nosoftlockup"
    ++  " intel_pstate=disable processor.max_cstate=1 intel_idle.max_cstate=0"
    ++  " nmi_watchdog=0 mce=ignore_ce"

let clocksourceText =
      \(tscReliable : Bool) ->
        if    tscReliable
        then  "clocksource=tsc"
        else  "clocksource=hpet"

let smiMitigationText =
      \(tscReliable : Bool) ->
        "${baseSMIMitigationText} ${clocksourceText tscReliable}"

let workloadDefaults =
      { isolatedCores = "2-7", housekeepingCores = "0-1", idlePoll = True }

let workloadIsolationText =
      \(isolatedCores : Text) ->
      \(housekeepingCores : Text) ->
      \(idlePoll : Bool) ->
        let base =
                "isolcpus=managed_irq,domain,${isolatedCores}"
            ++  " nohz_full=${isolatedCores}"
            ++  " rcu_nocbs=${isolatedCores}"
            ++  " irqaffinity=${housekeepingCores}"

        let idle = if idlePoll then " idle=poll" else ""

        in  "${base}${idle}"

let runtimeIsolationText =
      \(housekeepingCores : Text) ->
        "skew_tick=1 rcu_nocb_poll nowatchdog kthread_cpus=${housekeepingCores}"

let gpuDisplayText = "amdgpu.modeset=1 amdgpu.dc=1 amdgpu.dcdebugmask=0x10"

let debugText = "earlyprintk=vga,keep ignore_loglevel initcall_debug nosmp nosoftlockup"

in  { baseSMIMitigationText
    , clocksourceText
    , smiMitigationText
    , workloadDefaults
    , workloadIsolationText
    , runtimeIsolationText
    , gpuDisplayText
    , debugText
    }
