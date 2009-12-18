

text = multiname_kind_RTQName
textLines = text.split("\n")



def parseLine(line)
	parts = line.split(" ")
	
	if(parts.length == 2)
		if(line.include?("["))
			 "    #{parts[1].split("[")[0]}s :: [#{parts[0]}]"
		else
			"    #{parts[1]} :: #{parts[0]}"
		end
		
	else
		line
	end
end

textLines.each_with_index{|line, index|
	newLine = parseLine(line)
	
	if(index < textLines.length - 2 && index > 1 )
		puts "#{newLine},"
	elsif(index == 0)
		puts "data #{newLine.chomp} = #{newLine.chomp}"
	else
		puts "#{newLine}"
	end	
}

