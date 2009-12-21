File.open("c:\\Git\\haskellCrap\\functionalProgrammingPeople.txt", "r").each{|line|
	if(line.include?("Name:"))
		puts line.gsub("Name:", "").chomp
	elsif(line.include?(":") == false &&
		line.split(" ").length == 2)
		puts line.chomp
	end

}