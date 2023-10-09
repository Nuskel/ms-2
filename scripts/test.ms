import misc.util
import misc.regex
import network as net

proto Location:

  member world
  member x, y, z

  construct(string encoded):
    once pattern = regex.compile('(.+),(\\d+),(\\d+),(\\d+)')

    with groups = pattern.match(encoded):
      self.world = groups[0]
      self.x = number(groups[1])
      self.y = number(groups[2])
      self.z = number(groups[3])
    end
  end

end

proto NetPlayer as net.NetPeer:

  member name
  member location

  construct():
    self.NetPeer.handlers.handshake.add(self.__read_data)
  end

  def __read_data(net.Handshake handshake):
    self.name = handshake.userdata.player.name
    self.location = Location(handshake.userdata.player.location)
  end

end

def connect(string host, string username):
  net.peer = NetPlayer()

  with connection = net.connect(host, 8701, userdata={ name: username }):
    net.state.write(connection)
    return (NetPlayer) net.peer
  end

  throw net.error
end