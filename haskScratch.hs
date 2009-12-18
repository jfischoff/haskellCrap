
main = do
	let text = "{\
\u30 int_count\
\s32 integer[int_count]\
\u30 uint_count\
\u32 uinteger[uint_count]\
\u30 double_count\
\d64 double[double_count]\
\u30 string_count\
\string_info string[string_count]\
\u30 namespace_count\
\namespace_info namespace[namespace_count]\
\u30 ns_set_count\
\ns_set_info ns_set[ns_set_count]\
\u30 multiname_count\
\multiname_info multiname[multiname_count]\
\}"
	--print $ splitAndRearrange text
	print $ splitBySpace "fda fdas"
	
splitAndRearrange text = result where
	parts = lines text
	result = map ( ) parts
	
splitBySpace line = break (" " == ) line
	