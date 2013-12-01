task default: :server

desc 'Start the Jekyll server'
task :server, [:host, :port] do |t, args|
  args.with_defaults(host: '0.0.0.0', port: '4000')
  sh('bundle', 'exec', 'jekyll', 'serve',
    '--drafts', '--future', '--watch',
    '--host', args.host, '--port', args.port)
end

desc 'Generate static assets'
multitask assets: [:images, '404.html', 'static/scripts/all.min.js', 'static/styles/all.min.css']

file '404.html' do |t|
  sh(*%w(bundle exec jekyll build))
  sh('cp', File.join('_site', '404', 'index.html'), '404.html')
end

file 'static/scripts/all.min.js' do |t|
  sh('yui-compressor', '-o', t.name, 'static/scripts/main.js')
end

file 'static/styles/all.min.css' => ['static/styles/reset.css', 'static/styles/main.css', 'static/styles/syntax.css'] do |t|
  sh("cat #{t.prerequisites.join(' ')} | yui-compressor --type css -o #{t.name}")
end

desc 'Generate rasterized images'
multitask :images

def image(name, width, height = width, background: '#ac4142', rotate: true)
  "static/images/#{name}.png".tap do |path|
    args = [
      '-filter', 'point',
      '-background', background,
      '-density', ([width, height].min * 72.0 / 80.0).round(1).to_s,
      '-gravity', 'center'
    ]

    if rotate && width > height
      args += ['-rotate', '90']
      width, height = height, width
    end

    args += [
      '-extent', "#{width}x#{height}",
      'static/images/logo.svg', path
    ]

    task images: [path]
    file path do
      sh('convert', *args)
    end
  end
end

image('og-image', 300)
image('msapplication-TileImage', 144, background: 'none')
image('msapplication-wide310x150logo', 310, 150, background: 'none', rotate: false)
[70, 150, 310].each do |size|
  image("msapplication-square#{size}x#{size}logo", size, background: 'none')
end
[16, 32, 48, 256].each do |size|
  image("favicon-#{size}", size)
end
[57, 60, 72, 76, 114, 120, 144, 152].each do |size|
  image("apple-touch-icon-#{size}x#{size}", size)
end
[[320, 460], [640, 920], [640, 1096], [1024, 748], [768, 1004], [2048, 1496], [1536, 2008]].each do |(width, height)|
  size = width > height ? "#{height}x#{width}" : "#{width}x#{height}"
  image("apple-touch-startup-image-#{size}", width, height)
end

file 'static/images/favicon.ico' do
  sh('convert -colors 4 static/images/favicon-*.png static/images/favicon.ico')
end
task images: ['static/images/favicon.ico']
