task default: [:build]

desc 'Build the site'
task :build do
  sh(*%w(jekyll build))
end

desc 'Serve the site'
task :serve do
  sh(*%w(jekyll serve --drafts --future --incremental --watch))
end

desc 'Remove assets'
task :clean do
  sh(*%w(rm -f -r
    _site/
    static/images/favicon-16.png
    static/images/favicon-256.png
    static/images/favicon-32.png
    static/images/favicon-48.png
    static/images/favicon.ico
    static/images/og-image.png
  ))
end

desc 'Rasterize images'
multitask :images
def image(name, width, height = width, background: '#f5f5f5', rotate: true)
  "static/images/#{name}.png".tap do |path|
    args = [
      '-filter', 'point',
      '-background', background,
      '-density', ([width, height].min * 72.0 / 7.0).round(1).to_s,
      '-gravity', 'center'
    ]

    if rotate && width > height
      args += ['-rotate', '90']
      width, height = height, width
    end

    args += [
      '-extent', "#{width}x#{height}",
      '_includes/logo.svg', path
    ]

    task images: [path]
    file path do
      sh('convert', *args)
    end
  end
end

image('og-image', 300)

favicons = []
[16, 32, 48, 256].each do |size|
  favicons << image("favicon-#{size}", size)
end
file 'static/images/favicon.ico' => favicons do |t|
  sh("convert -colors 16 #{t.prerequisites.join(' ')} #{t.name}")
end
task images: ['static/images/favicon.ico']
