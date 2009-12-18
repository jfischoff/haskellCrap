
data u16 = u16 Int
data u30 = u30 Int
data s32 = s32 Int

CONSTANT_QName = 0x07
CONSTANT_QNameA  = 0x0D
CONSTANT_RTQName  = 0x0F
CONSTANT_RTQNameA  = 0x10
CONSTANT_RTQNameL  = 0x11
CONSTANT_RTQNameLA =  0x12
CONSTANT_Multiname =  0x09
CONSTANT_MultinameA  = 0x0E
CONSTANT_MultinameL =  0x1B
CONSTANT_MultinameLA =  0x1C

NEED_ARGUMENTS = 0x01--Suggests to the run-time that an “arguments” object (as specified by the
					--ActionScript 3.0 Language Reference) be created. Must not be used
					--together with NEED_REST. See Chapter 3.
NEED_ACTIVATION = 0x02 -- Must be set if this method uses the newactivation opcode.
NEED_REST = 0x04 --This flag creates an ActionScript 3.0 rest arguments array. Must not be
				--used with NEED_ARGUMENTS. See Chapter 3.
HAS_OPTIONAL = 0x08 --Must be set if this method has optional parameters and the options
					--field is present in this method_info structure.
SET_DXNS  = 0x40 --Must be set if this method uses the dxns or dxnslate opcodes.
HAS_PARAM_NAMES =  0x80 --

data abcFile = {minor_version::u16, major_version::u16,
	constant_pool :: cpool_info, 
	method_count :: u30,
	methods :: [method_info],
	metadata_count :: u30,
	metadata[metadata_count] :: metadata_info,
	class_count :: u30,
	instances :: [instance_info],
	classes :: [class_info],
	script_count :: u30
	scripts::[script_info]
	method_body_count::u30
	method_body::[method_body_info]}
	
data cpool_info = cpool_info
{
    int_count :: u30,
    integers :: [s32],
    uint_count :: u30,
    uintegers :: [u32],
    double_count :: u30,
    doubles :: [d64],
    string_count :: u30,
    strings :: [string_info],
    namespace_count :: u30,
    namespaces :: [namespace_info],
    ns_set_count :: u30,
    ns_sets :: [ns_set_info],
    multiname_count :: u30,
    multinames :: [multiname_info]
}

data string_info = string_info
{
    size :: u30,
    utf8s :: [u8]
}

data namespace_info = namespace_info
{
    kind :: u8,
    name :: u30
}

data multiname_info = multiname_info
{
    kind :: u8,
    datas :: [u8]
}

data multiname_kind_QName = multiname_kind_QName
{
    ns :: u30,
    name :: u30
}