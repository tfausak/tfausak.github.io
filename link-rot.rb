#!/usr/bin/env ruby
require 'net/http'

pattern = /^\[[0-9]+\]: (http.*)$/i
Dir.glob('./_posts/*.md').sort.each do |file|
  puts file

  File.open(file).each_line do |line|
    match = pattern.match(line)
    if match
      uri = URI(match[1])
      Net::HTTP.start(
        uri.host,
        uri.port,
        :use_ssl => uri.scheme == 'https'
      ) do |http|
        request = Net::HTTP::Head.new(uri.request_uri)
        response = http.request(request)

        case response
        when Net::HTTPOK then
          puts "\033[32m\u2713 #{uri}\033[0m"
        else
          puts "\033[31m\u2716 #{uri}\033[0m"
        end
      end

      sleep(1)
    end
  end
end
