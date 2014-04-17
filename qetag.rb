require 'digest'
require 'base64'

BLOCK_SIZE = 2 ** 22

def qetag(file_name)
  sha1 = []
  open(file_name, "rb") do |f|
    until f.eof?
      chunk = f.read(BLOCK_SIZE)
      sha1 << Digest::SHA1.digest(chunk)
    end
  end

  if sha1.size == 1
    Base64.urlsafe_encode64(0x16.chr + sha1[0])
  else
    Base64.urlsafe_encode64(0x96.chr + Digest::SHA1.digest(sha1.join))
  end 
end

puts qetag ARGV[0]
