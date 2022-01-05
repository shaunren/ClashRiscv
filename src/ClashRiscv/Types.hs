module ClashRiscv.Types where

import Clash.Prelude ( Index, Signed, Unsigned )

type NBits = 32
type NRegisters = 32

type Register = Index NRegisters
type RegAddr = Unsigned 5
type RegReadAddr  = RegAddr
type RegWriteAddr = RegAddr
type Addr  = Unsigned NBits
type Value = Unsigned NBits
type SignedValue = Signed NBits
