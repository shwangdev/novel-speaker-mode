#
import vlc, time

from rpyc import Service
from rpyc.utils.server import ThreadedServer

class PlayerService(Service):
    
    def exposed_play_mp3(self, uri):
        print (uri)
        try: 
            player = vlc.MediaPlayer(uri)
            player.play()
            time.sleep(3)
            while True:
                if player.is_playing() == 1:
                    pass
                else:
                    print("play done")
                    break
        except Exception as e:
            print(e)
       
t = ThreadedServer(PlayerService, port = 18861)
t.start()
