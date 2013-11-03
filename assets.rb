# Use this script together with an image optimization program (like ImageOptim)
# to generate rasterized logo assets. The only requirement is ImageMagick.

# Path to images directory.
$directory = './static/images'

# Path to source SVG file.
$input = "#{$directory}/logo.svg"

# Background color.
$background = '#ac4142'

# ImageMagick resizes vector images based on density, not desired size. This
# function takes the desired size and returns the density needed.
def density(size)
  # ImageMagick generates images with 72 pixels per inch. The SVG logo has a
  # nominal size of 80 pixels square.
  ratio = 72.0 / 80.0

  # Without rounding, it's possible for the rasterized image to be off by one
  # pixel.
  (size * ratio).round(1)
end

# Print a command before running it with `system`.
def run(*args)
  puts "\e[37m#{args.join(' ')}\e[0m"
  system(*args.map(&:to_s))
end

# Rasterize the logo as a square.
def square(size, slug, background = $background)
  output = "#{$directory}/#{slug}.png"
  run('convert',
    '-background', background,
    '-density', density(size),
    $input, output)
  output
end

# Rasterize the logo as a rectangle.
def rect(width, height, slug, rotate = true, background = $background)
  output = "#{$directory}/#{slug}.png"
  args = [
    '-background', background,
    '-density', density([width, height].min),
    '-gravity', 'center'
  ]

  if rotate && width > height
    args += ['-rotate', 90]
    width, height = height, width
  end

  run('convert', *args, '-extent', "#{width}x#{height}", $input, output)
  output
end

# [57, 60, 72, 76, 114, 120, 144, 152].each do |size|
#   square(size, "apple-touch-icon-#{size}x#{size}")
# end
[70, 150, 310].each do |size|
  square(size, "msapplication-square#{size}x#{size}logo", 'none')
end
[144].each do |size|
  square(size, 'msapplication-TileImage', 'none')
end
# [300].each do |size|
#   square(size, 'og-image')
# end
# [[320, 460], [640, 920], [640, 1096], [1024, 748], [768, 1004], [2048, 1496], [1536, 2008]].each do |(width, height)|
#   size = width > height ? "#{height}x#{width}" : "#{width}x#{height}"
#   rect(width, height, "apple-touch-startup-image-#{size}")
# end
[[310, 150]].each do |(width, height)|
  rect(width, height, "msapplication-wide#{width}x#{height}logo", false, 'none')
end

# icons = [16, 32, 48, 256].map do |size|
#   square(size, "favicon-#{size}")
# end
# icons << "#{$directory}/favicon.ico"
# run('convert', '-colors', 4, *icons)
