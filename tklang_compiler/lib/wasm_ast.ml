type typeIdx = TypeIdx of int

type funcIdx = FuncIdx of int

type tableIdx = TableIdx of int

type memIdx = MemIdx of int

type globalIdx = GlobalIdx of int

type localIdx = LocalIdx of int

type labelIdx = LabelIdx of int

type valType = I32 | I64 | F32 | F64

type funcType = {
  params: valType list;
  results: valType list;
}

type limits = {
  min: int;
  max: int option; 
}

type memType = {
  limits: limits;
}

type elemType = FuncRef

type tableType = {
  limits: limits;
  elemType: elemType;
}

type globalMutability = Mutable | Const

  
type globalType = {
  mutability: globalMutability;
  valType: valType;
}

type blockType = BlockType of valType option

type memarg = {
  offset: int;
  align: int
}

type instr =
  | I32Const of int
  | I64Const of int
  (*
  | F32Const of float
  | F64Const of float
  *)
  | I32Clz
  | I32Ctz
  | I32Popcnt
  | I64Clz
  | I64Ctz
  | I64Popcnt
  (*
  | F32Abs
  | F32Neg
  | F32Sqrt
  | F32Ceil
  | F32Floor
  | F32Trunc
  | F32Nearest
  | F64Abs
  | F64Neg
  | F64Sqrt
  | F64Ceil
  | F64Floor
  | F64Trunc
  | F64Nearest
  *)
  | I32Add
  | I32Sub
  | I32Mul
  | I32DivS
  | I32DivU
  | I32RemS
  | I32RemU
  | I32And
  | I32Or
  | I32Xor
  | I32ShL
  | I32ShrS
  | I32ShrU
  | I32Rotl
  | I32Rotr
  | I64Add
  | I64Sub
  | I64Mul
  | I64DivS
  | I64DivU
  | I64RemS
  | I64RemU
  | I64And
  | I64Or
  | I64Xor
  | I64ShL
  | I64ShrS
  | I64ShrU
  | I64Rotl
  | I64Rotr
  (*
  | F32Add
  | F32Sub
  | F32Mul
  | F32Div
  | F32Min
  | F32Max
  | F32CopySign
  | F64Add
  | F64Sub
  | F64Mul
  | F64Div
  | F64Min
  | F64Max
  | F64CopySign
  *)
  | I32Eqz
  | I64Eqz
  | I32Eq
  | I32Ne
  | I32LtS
  | I32LtU
  | I32GtS
  | I32GtU
  | I32LeS
  | I32LeU
  | I32GeS
  | I32GeU
  | I64Eq
  | I64Ne
  | I64LtS
  | I64LtU
  | I64GtS
  | I64GtU
  | I64LeS
  | I64LeU
  | I64GeS
  | I64GeU
  (*
  | F32Eq
  | F32Ne
  | F32Lt
  | F32Gt
  | F32Le
  | F32Ge
  | F64Eq
  | F64Ne
  | F64Lt
  | F64Gt
  | F64Le
  | F64Ge
  *)
  | I32WrapI64
  | I64ExtendI32S
  | I64ExtendI32U
  (*
  | I32TruncF32S
  | I32TruncF32U
  | I32TruncF64S
  | I32TruncF64U
  | I64TruncF32S
  | I64TruncF32U
  | I64TruncF64S
  | I64TruncF64U
  | F32DemoteF64
  | F64PromoteF32
  | F32ConvertI32S
  | F32ConvertI32U
  | F32ConvertI64S
  | F32ConvertI64U
  | F64ConvertI32S
  | F64ConvertI32U
  | F64ConvertI64S
  | F64ConvertI64U
  | I32ReinterpretF32
  | I64ReinterpretF64
  | F32ReinterpretI32
  | F64ReinterpretI64
  *)
  | Drop
  | Select
  | LocalGet of localIdx
  | LocalSet of localIdx
  | LocalTee of localIdx
  | GlobalGet of globalIdx
  | GlobalSet of globalIdx
  | I32Load of memarg
  | I64Load of memarg
  (*
  | F32Load of memarg
  | F64Load of memarg
  *)
  | I32Store of memarg
  | I64Store of memarg
  (*
  | F32Store of memarg
  | F64Store of memarg
  *)
  | I32Load8S of memarg
  | I32Load8U of memarg
  | I64Load8S of memarg
  | I64Load8U of memarg
  | I32Load16S of memarg
  | I32Load16U of memarg
  | I64Load16S of memarg
  | I64Load16U of memarg
  | I64Load32S of memarg
  | I64Load32U of memarg
  | I32Store8 of memarg
  | I64Store8 of memarg
  | I32Store16 of memarg
  | I64Store16 of memarg
  | I64Store32 of memarg
  | MemorySize
  | MemoryGrow
  | Nop
  | Unreachable
  | Block of blockType * instr list
  | Loop of blockType * instr list
  | If of blockType * instr list * instr list
  | Br of labelIdx
  | BrIf of labelIdx
  | BrTable of labelIdx list * labelIdx
  | Return
  | Call of funcIdx
  | CallIndirect of typeIdx

type expr = Expr of instr list

type func = {
 typ: typeIdx;
 locals: valType list;
 body: expr; 
}

type table = {
  typ: tableType
}

type mem = {
  typ: memType
}

type global = {
  typ: globalType;
  init: expr
}

type elem = {
  table: tableIdx;
  offset: expr;
  init: funcIdx list
}

type byte = char

type data = {
  data: memIdx;
  offset: expr;
  init: byte list
}

type start = {
  func: funcIdx
}

type exportDesc = 
  | Func of funcIdx
  | Table of tableIdx
  | Mem of memIdx
  | Global of globalIdx

type export = {
  name: string;
  desc: exportDesc
}

type importDesc = 
  | Func of funcType
  | Table of tableType
  | Mem of memType
  | Global of globalType

type import = {
  module_: string;
  name: string;
  desc: importDesc
}

type module_ = {
  types: funcType list;
  funcs: func list;
  tables: table list;
  mems: mem list;
  globals: global list;
  elems: elem list;
  data: data list;
  start: start;
  exports: export list;
  imports: import list
}
