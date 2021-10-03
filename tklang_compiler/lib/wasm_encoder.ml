let encode_byte (value: Wasm_ast.byte): Wasm_ast.byte Diff_list.t =
    Diff_list.singleton value

let rec encode_unsigned_integer_helper (value: int) (bytes: Wasm_ast.byte Diff_list.t): Wasm_ast.byte Diff_list.t =
  let byte = value land 0x7f in
  let value = value lsr 7 in
  let byte = Char.chr(
      if value == 0 then
        byte
      else
        byte lor 0x80
    ) in
  let bytes = Diff_list.snoc bytes byte in
  if value == 0 then
    bytes
  else
    encode_unsigned_integer_helper value bytes


let encode_unsigned_integer (value: int): Wasm_ast.byte Diff_list.t =
  encode_unsigned_integer_helper value Diff_list.empty

let rec encode_signed_integer_helper (size: int) (value: int) (neg: bool) (bytes: Wasm_ast.byte Diff_list.t): Wasm_ast.byte Diff_list.t =
  let byte = value land 0x7f in
  let value = value lsr 7 in
  let value = if neg then value lor ((Int.neg 0) lsl (size - 7)) else value in
  if ((value == 0 && byte land 0x40 == 0) || (value == -1 && byte land 0x40 != 0)) then
    bytes
  else
    let bytes = Diff_list.snoc bytes (Char.chr byte) in
    encode_signed_integer_helper size value neg bytes

let encode_signed_integer (size: int) (value: int): Wasm_ast.byte Diff_list.t =
  encode_signed_integer_helper size value (value < 0) Diff_list.empty

let encode_vec (encoder: 'a -> Wasm_ast.byte Diff_list.t) (xs: 'a list): Wasm_ast.byte Diff_list.t =
  Diff_list.append (encode_unsigned_integer (List.length xs)) (Diff_list.concat (List.map encoder xs))

let encode_memarg (memarg: Wasm_ast.memarg): Wasm_ast.byte Diff_list.t =
  Diff_list.append (encode_unsigned_integer memarg.align) (encode_unsigned_integer memarg.offset)

let encode_val_type (val_type: Wasm_ast.valType): Wasm_ast.byte Diff_list.t =
  match val_type with
  | Wasm_ast.I32 -> Diff_list.singleton (Char.chr 0x7f)
  | Wasm_ast.I64 -> Diff_list.singleton (Char.chr 0x7e)
  | Wasm_ast.F32 -> Diff_list.singleton (Char.chr 0x7d)
  | Wasm_ast.F64 -> Diff_list.singleton (Char.chr 0x7c)

let encode_block_type (Wasm_ast.BlockType block_type: Wasm_ast.blockType): Wasm_ast.byte Diff_list.t =
  match block_type with
  | Some val_type -> encode_val_type val_type
  | None -> Diff_list.singleton (Char.chr 0x40)

let rec encode_instr (instr: Wasm_ast.instr): Wasm_ast.byte Diff_list.t =
  match instr with
    | Wasm_ast.Unreachable -> Diff_list.singleton (Char.chr 0x00)
    | Wasm_ast.Nop -> Diff_list.singleton (Char.chr 0x01)
    | Wasm_ast.Block(bt, is) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x02);
        encode_block_type bt;
        List.map encode_instr is |> Diff_list.concat;
        Diff_list.singleton (Char.chr 0x0b)
      ]
    | Wasm_ast.Loop(bt, is) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x03);
        encode_block_type bt;
        List.map encode_instr is |> Diff_list.concat;
        Diff_list.singleton (Char.chr 0x0b);
      ]
    | Wasm_ast.If(bt, is1, is2) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x04);
        encode_block_type bt;
        List.map encode_instr is1 |> Diff_list.concat;
        Diff_list.singleton (Char.chr 0x05);
        List.map encode_instr is2 |> Diff_list.concat;
        Diff_list.singleton (Char.chr 0x0b)
      ]
    | Wasm_ast.Br (Wasm_ast.LabelIdx l) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x0c);
        encode_unsigned_integer l;
      ]
    | Wasm_ast.BrIf (Wasm_ast.LabelIdx l) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x0d);
        encode_unsigned_integer l;
      ]
    | Wasm_ast.BrTable(ls, Wasm_ast.LabelIdx l) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x0e);
        encode_vec encode_unsigned_integer (List.map (fun (Wasm_ast.LabelIdx x) -> x) ls);
        encode_unsigned_integer l;
      ]
    | Wasm_ast.Return -> Diff_list.singleton (Char.chr 0x0f)
    | Wasm_ast.Call (Wasm_ast.FuncIdx l) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x10);
        encode_unsigned_integer l;
      ]
    | Wasm_ast.CallIndirect (Wasm_ast.TypeIdx t) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x11);
        encode_unsigned_integer t;
        Diff_list.singleton (Char.chr 0x00);
      ]
    | Wasm_ast.Drop -> Diff_list.singleton (Char.chr 0x1a)
    | Wasm_ast.Select -> Diff_list.singleton (Char.chr 0x1b)
    | Wasm_ast.LocalGet (Wasm_ast.LocalIdx l) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x20);
        encode_unsigned_integer l;
      ]
    | Wasm_ast.LocalSet (Wasm_ast.LocalIdx l) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x21);
        encode_unsigned_integer l;
      ]
    | Wasm_ast.LocalTee (Wasm_ast.LocalIdx l) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x22);
        encode_unsigned_integer l;
      ]
    | Wasm_ast.GlobalGet (Wasm_ast.GlobalIdx g) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x23);
        encode_unsigned_integer g;
      ]
    | Wasm_ast.GlobalSet (Wasm_ast.GlobalIdx g) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x24);
        encode_unsigned_integer g;
      ]
    | Wasm_ast.I32Load (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x28);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Load (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x29);
        encode_memarg memarg;
      ]
    | Wasm_ast.I32Load8S (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x2c);
        encode_memarg memarg;
      ]
    | Wasm_ast.I32Load8U (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x2d);
        encode_memarg memarg;
      ]
    | Wasm_ast.I32Load16S (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x2e);
        encode_memarg memarg;
      ]
    | Wasm_ast.I32Load16U (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x2f);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Load8S (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x30);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Load8U (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x31);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Load16S (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x32);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Load16U (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x33);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Load32S (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x34);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Load32U (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x35);
        encode_memarg memarg;
      ]
    | Wasm_ast.I32Store (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x36);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Store (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x37);
        encode_memarg memarg;
      ]
    | Wasm_ast.I32Store8 (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x3a);
        encode_memarg memarg;
      ]
    | Wasm_ast.I32Store16 (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x3b);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Store8 (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x3c);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Store16 (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x3d);
        encode_memarg memarg;
      ]
    | Wasm_ast.I64Store32 (memarg) -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x3e);
        encode_memarg memarg;
      ]
    | Wasm_ast.MemorySize -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x3f);
        Diff_list.singleton (Char.chr 0x00);
      ]
    | Wasm_ast.MemoryGrow -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x40);
        Diff_list.singleton (Char.chr 0x00);
      ]
    | Wasm_ast.I32Const x -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x41);
        encode_signed_integer 32 x;
      ]
    | Wasm_ast.I64Const x -> Diff_list.concat [
        Diff_list.singleton (Char.chr 0x42);
        encode_signed_integer 64 x;
      ]
    | Wasm_ast.I32Eqz -> Diff_list.singleton (Char.chr 0x45)
    | Wasm_ast.I32Eq -> Diff_list.singleton (Char.chr 0x46)
    | Wasm_ast.I32Ne -> Diff_list.singleton (Char.chr 0x47)
    | Wasm_ast.I32LtS -> Diff_list.singleton (Char.chr 0x48)
    | Wasm_ast.I32LtU -> Diff_list.singleton (Char.chr 0x49)
    | Wasm_ast.I32GtS -> Diff_list.singleton (Char.chr 0x4a)
    | Wasm_ast.I32GtU -> Diff_list.singleton (Char.chr 0x4b)
    | Wasm_ast.I32LeS -> Diff_list.singleton (Char.chr 0x4c)
    | Wasm_ast.I32LeU -> Diff_list.singleton (Char.chr 0x4d)
    | Wasm_ast.I32GeS -> Diff_list.singleton (Char.chr 0x4e)
    | Wasm_ast.I32GeU -> Diff_list.singleton (Char.chr 0x4f)
    | Wasm_ast.I64Eqz -> Diff_list.singleton (Char.chr 0x50)
    | Wasm_ast.I64Eq -> Diff_list.singleton (Char.chr 0x51)
    | Wasm_ast.I64Ne -> Diff_list.singleton (Char.chr 0x52)
    | Wasm_ast.I64LtS -> Diff_list.singleton (Char.chr 0x53)
    | Wasm_ast.I64LtU -> Diff_list.singleton (Char.chr 0x54)
    | Wasm_ast.I64GtS -> Diff_list.singleton (Char.chr 0x55)
    | Wasm_ast.I64GtU -> Diff_list.singleton (Char.chr 0x56)
    | Wasm_ast.I64LeS -> Diff_list.singleton (Char.chr 0x57)
    | Wasm_ast.I64LeU -> Diff_list.singleton (Char.chr 0x58)
    | Wasm_ast.I64GeS -> Diff_list.singleton (Char.chr 0x59)
    | Wasm_ast.I64GeU -> Diff_list.singleton (Char.chr 0x5a)
    | Wasm_ast.I32Clz -> Diff_list.singleton (Char.chr 0x67)
    | Wasm_ast.I32Ctz -> Diff_list.singleton (Char.chr 0x68)
    | Wasm_ast.I32Popcnt -> Diff_list.singleton (Char.chr 0x69)
    | Wasm_ast.I32Add -> Diff_list.singleton (Char.chr 0x6a)
    | Wasm_ast.I32Sub -> Diff_list.singleton (Char.chr 0x6b)
    | Wasm_ast.I32Mul -> Diff_list.singleton (Char.chr 0x6c)
    | Wasm_ast.I32DivS -> Diff_list.singleton (Char.chr 0x6d)
    | Wasm_ast.I32DivU -> Diff_list.singleton (Char.chr 0x6e)
    | Wasm_ast.I32RemS -> Diff_list.singleton (Char.chr 0x6f)
    | Wasm_ast.I32RemU -> Diff_list.singleton (Char.chr 0x70)
    | Wasm_ast.I32And -> Diff_list.singleton (Char.chr 0x71)
    | Wasm_ast.I32Or -> Diff_list.singleton (Char.chr 0x72)
    | Wasm_ast.I32Xor -> Diff_list.singleton (Char.chr 0x73)
    | Wasm_ast.I32ShL -> Diff_list.singleton (Char.chr 0x74)
    | Wasm_ast.I32ShrS -> Diff_list.singleton (Char.chr 0x75)
    | Wasm_ast.I32ShrU -> Diff_list.singleton (Char.chr 0x76)
    | Wasm_ast.I32Rotl -> Diff_list.singleton (Char.chr 0x77)
    | Wasm_ast.I32Rotr -> Diff_list.singleton (Char.chr 0x78)
    | Wasm_ast.I64Clz -> Diff_list.singleton (Char.chr 0x79)
    | Wasm_ast.I64Ctz -> Diff_list.singleton (Char.chr 0x7a)
    | Wasm_ast.I64Popcnt -> Diff_list.singleton (Char.chr 0x7b)
    | Wasm_ast.I64Add -> Diff_list.singleton (Char.chr 0x7c)
    | Wasm_ast.I64Sub -> Diff_list.singleton (Char.chr 0x7d)
    | Wasm_ast.I64Mul -> Diff_list.singleton (Char.chr 0x7e)
    | Wasm_ast.I64DivS -> Diff_list.singleton (Char.chr 0x7f)
    | Wasm_ast.I64DivU -> Diff_list.singleton (Char.chr 0x80)
    | Wasm_ast.I64RemS -> Diff_list.singleton (Char.chr 0x81)
    | Wasm_ast.I64RemU -> Diff_list.singleton (Char.chr 0x82)
    | Wasm_ast.I64And -> Diff_list.singleton (Char.chr 0x83)
    | Wasm_ast.I64Or -> Diff_list.singleton (Char.chr 0x84)
    | Wasm_ast.I64Xor -> Diff_list.singleton (Char.chr 0x85)
    | Wasm_ast.I64ShL -> Diff_list.singleton (Char.chr 0x86)
    | Wasm_ast.I64ShrS -> Diff_list.singleton (Char.chr 0x87)
    | Wasm_ast.I64ShrU -> Diff_list.singleton (Char.chr 0x88)
    | Wasm_ast.I64Rotl -> Diff_list.singleton (Char.chr 0x89)
    | Wasm_ast.I64Rotr -> Diff_list.singleton (Char.chr 0x8a)
    | Wasm_ast.I32WrapI64 -> Diff_list.singleton (Char.chr 0xa7)
    | Wasm_ast.I64ExtendI32S -> Diff_list.singleton (Char.chr 0xac)
    | Wasm_ast.I64ExtendI32U -> Diff_list.singleton (Char.chr 0xad)

let encode_expr (Expr is: Wasm_ast.expr):  Wasm_ast.byte Diff_list.t =
  Diff_list.concat [
    Diff_list.concat (List.map encode_instr is);
    Diff_list.singleton (Char.chr 0x0b);
  ]

let encode_name (s: string):  Wasm_ast.byte Diff_list.t =
  Bytes.of_string s |> Bytes.to_seq |> List.of_seq |> encode_vec encode_byte

let encode_func_type (value: Wasm_ast.funcType):  Wasm_ast.byte Diff_list.t =
  Diff_list.concat [
    Diff_list.singleton (Char.chr 0x60);
    encode_vec encode_val_type value.params;
    encode_vec encode_val_type value.results;
  ]

let encode_limits (value: Wasm_ast.limits):  Wasm_ast.byte Diff_list.t =
  match value.max with
  | None -> Diff_list.concat [
    Diff_list.singleton (Char.chr 0x00);
    encode_unsigned_integer value.min;
  ]
  | Some max -> Diff_list.concat [
    Diff_list.singleton (Char.chr 0x01);
    encode_unsigned_integer value.min;
    encode_unsigned_integer max;
  ]

let encode_mem_type (value: Wasm_ast.memType):  Wasm_ast.byte Diff_list.t =
  encode_limits value.limits

let encode_elem_type (value: Wasm_ast.elemType):  Wasm_ast.byte Diff_list.t =
  match value with
  | Wasm_ast.FuncRef -> Diff_list.singleton (Char.chr 0x70)

let encode_table_type (value: Wasm_ast.tableType):  Wasm_ast.byte Diff_list.t =
  Diff_list.concat [
    encode_elem_type value.elemType;
    encode_limits value.limits;
  ]

let encode_mutability (value: Wasm_ast.globalMutability):  Wasm_ast.byte Diff_list.t =
  match value with
  | Wasm_ast.Const -> Diff_list.singleton (Char.chr 0x00)
  | Wasm_ast.Mutable -> Diff_list.singleton (Char.chr 0x01)

let encode_global_type (value: Wasm_ast.globalType):  Wasm_ast.byte Diff_list.t =
  Diff_list.concat [
    encode_val_type value.valType;
    encode_mutability value.mutability;
  ]

let encode_import_desc (value: Wasm_ast.importDesc):  Wasm_ast.byte Diff_list.t =
  match value with
  | Wasm_ast.Func (Wasm_ast.TypeIdx x) -> Diff_list.concat [
      Diff_list.singleton (Char.chr 0x00);
      encode_unsigned_integer x;
    ]
  | Wasm_ast.Table x -> Diff_list.concat [
      Diff_list.singleton (Char.chr 0x01);
      encode_table_type x;
    ]
  | Wasm_ast.Mem x -> Diff_list.concat [
      Diff_list.singleton (Char.chr 0x02);
      encode_mem_type x;
    ]
  | Wasm_ast.Global x -> Diff_list.concat [
      Diff_list.singleton (Char.chr 0x03);
      encode_global_type x;
    ]

let encode_import (value: Wasm_ast.import):  Wasm_ast.byte Diff_list.t =
  Diff_list.concat [
    encode_name value.module_;
    encode_name value.name;
    encode_import_desc value.desc;
  ]

let encode_table (value: Wasm_ast.table):  Wasm_ast.byte Diff_list.t =
  Diff_list.concat [
    encode_table_type value.typ;
  ]

let encode_mem (value: Wasm_ast.mem):  Wasm_ast.byte Diff_list.t =
  Diff_list.concat [
    encode_mem_type value.typ;
  ]

let encode_global (value: Wasm_ast.global):  Wasm_ast.byte Diff_list.t =
  Diff_list.concat [
    encode_global_type value.typ;
    encode_expr value.init;
  ]
  
let encode_export_desc (value: Wasm_ast.exportDesc):  Wasm_ast.byte Diff_list.t =
  match value with
  | Wasm_ast.Func (Wasm_ast.FuncIdx x) -> Diff_list.concat [
      Diff_list.singleton (Char.chr 0x00);
      encode_unsigned_integer x;
    ]
  | Wasm_ast.Table (Wasm_ast.TableIdx x) -> Diff_list.concat [
      Diff_list.singleton (Char.chr 0x01);
      encode_unsigned_integer x;
    ]
  | Wasm_ast.Mem (Wasm_ast.MemIdx x) -> Diff_list.concat [
      Diff_list.singleton (Char.chr 0x02);
      encode_unsigned_integer x;
    ]
  | Wasm_ast.Global (Wasm_ast.GlobalIdx x) -> Diff_list.concat [
      Diff_list.singleton (Char.chr 0x03);
      encode_unsigned_integer x;
    ]

let encode_export (value: Wasm_ast.export):  Wasm_ast.byte Diff_list.t =
  Diff_list.concat [
    encode_name value.name;
    encode_export_desc value.desc;
  ]

let encode_start (value: Wasm_ast.start):  Wasm_ast.byte Diff_list.t =
  let (Wasm_ast.FuncIdx idx) = value.func in
  Diff_list.concat [
    encode_unsigned_integer idx;
  ]

let encode_elem (value: Wasm_ast.elem):  Wasm_ast.byte Diff_list.t =
  let (Wasm_ast.TableIdx idx) = value.table in
  Diff_list.concat [
    encode_unsigned_integer idx;
    encode_expr value.offset;
    encode_vec (fun (Wasm_ast.FuncIdx x) -> encode_unsigned_integer x) value.init;
  ]

let encode_data (value: Wasm_ast.data):  Wasm_ast.byte Diff_list.t =
  let (Wasm_ast.MemIdx idx) = value.data in
  Diff_list.concat [
    encode_unsigned_integer idx;
    encode_expr value.offset;
    encode_vec encode_byte value.init;
  ]
