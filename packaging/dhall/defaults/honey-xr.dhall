-- honey-xr.dhall — XR kernel boot configuration
--
-- Adds RT boot parameters, SMI mitigation, and CPU isolation to the
-- stock config. Used when installing kernel-xr on honey.
--
-- Validated against live BLS entry: 6.19.5-4.xr.el10 on honey (2026-03-16)

let BootEntry = (../types/BootEntry.dhall).BootEntry
let GrubDefaults = (../types/GrubDefaults.dhall).GrubDefaults
let DracutConfig = (../types/DracutConfig.dhall).DracutConfig
let FstabEntry = (../types/FstabEntry.dhall).FstabEntry
let BootGeneration = (../types/BootGeneration.dhall).BootGeneration
let Platform = ../Platform.dhall
let BootParams = ../BootParams.dhall

let stock = ./honey-stock.dhall

-- Swap UUID (same as stock — from blkid)
let swapUUID = "6e9a97e2-44b0-4a14-922b-26ced14feed6"

-- Boot parameters: stock base + reusable host timing posture + XR runtime
-- isolation extras. The Dell timing posture now comes from BootParams /
-- HostTiming instead of being handwritten inline here.
let xrOptions =
        "ro crashkernel=2G-64G:256M,64G-:512M"
    ++  " resume=UUID=${swapUUID}"
    ++  " rd.lvm.lv=rl00/root rd.lvm.lv=rl00/swap"
    ++  " ${BootParams.xrText Platform.dellT7810 BootParams.bciWorkload}"

let config
    : BootGeneration
    = { generation = 1
      , description = "XR kernel with RT params + SMI mitigation"
      , bootEntry =
        { title = "XR Kernel (6.19.5-5.xr.el10)"
        , version = "6.19.5-5.xr.el10"
        , linux = "/vmlinuz-6.19.5-5.xr.el10"
        , initrd = [ "/initramfs-6.19.5-5.xr.el10.img" ]
        , rootDevice = "/dev/mapper/rl00-root"
        , extraOptions = xrOptions
        , machineId = stock.bootEntry.machineId
        , grubClass = Some "kernel"
        }
      , grubDefaults = stock.grubDefaults
          // { cmdlineDefault = xrOptions }
      , dracutConfig =
        { name = "xr"
        , addDrivers = [ "nvme", "nvme_core", "amdgpu", "uio", "uio_pci_generic" ]
        , addModules = [ "lvm" ]
        , omitDrivers = [ "iTCO_wdt", "i2c_i801" ]
        , hostonly = True
        , earlyMicrocode = True
        , fipsModule = False
        }
      , fstabEntries = stock.fstabEntries
      , rootDevice = "/dev/mapper/rl00-root"
      , rootVG = "rl00"
      }

-- Safety invariants (checked at Dhall evaluation time)
-- GRUB cannot read thin LVM: Red Hat BZ#1164947 (2014, WONTFIX)
let _ = assert : config.rootVG === "rl00"
let _ = assert : config.grubDefaults.enableBLS === True

in  config
