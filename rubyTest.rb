require 'Treemap'
require 'Treemap/image_output.rb'

root = Treemap::Node.new

0.upto(100){|i|
	root.new_child(:size => rand)
}

output = Treemap::ImageOutput.new do |o|
	o.width = 800
	o.height = 600
end

output.to_png(root, "C:/Scripts/Haskell/test.png")