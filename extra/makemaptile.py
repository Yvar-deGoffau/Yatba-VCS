#!/bin/python

# 38|48 37|47  18 17 14|15 13|16 11 12  
# 34|35 33|36  28 27 24|25 23|26 21 22 
# 44|45 43|46  58 57 54|55 53|56 51 52  
# 31|41 32|42  68 67 64|65 63|66 61 62

import pygame,sys  

bytemapping={
 (3,8):(0,0),
 (4,8):(0,0),
 (3,7):(0,1),
 (4,7):(0,1),

 (6,8):(0,2),
 (6,7):(0,3),
 (6,4):(0,4),
 (6,5):(0,4),
 (6,3):(0,5),
 (6,6):(0,5),
 (6,1):(0,6),
 (6,2):(0,7),

 
 (4,4):(1,0),
 (4,5):(1,0),
 (4,3):(1,1),
 (4,6):(1,1),

 (5,8):(1,2),
 (5,7):(1,3),
 (5,4):(1,4),
 (5,5):(1,4),
 (5,3):(1,5),
 (5,6):(1,5),
 (5,1):(1,6),
 (5,2):(1,7),


 (3,4):(2,0),
 (3,5):(2,0),
 (3,3):(2,1),
 (3,6):(2,1),

 (2,8):(2,2),
 (2,7):(2,3),
 (2,4):(2,4),
 (2,5):(2,4),
 (2,3):(2,5),
 (2,6):(2,5),
 (2,1):(2,6),
 (2,2):(2,7),


 (3,1):(3,0),
 (4,1):(3,0),
 (3,2):(3,1),
 (4,2):(3,1),

 (1,8):(3,2),
 (1,7):(3,3),
 (1,4):(3,4),
 (1,5):(3,4),
 (1,3):(3,5),
 (1,6):(3,5),
 (1,1):(3,6),
 (1,2):(3,7),
  }

def main():
 pygame.init()
 d=pygame.display.set_mode((512,384))
 s=pygame.Surface((8,6))

 savebyte=[
  [0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0],]
 
 while 1:
  for event in pygame.event.get():
   if event.type==pygame.QUIT:
    for y in range(6):
     txt=""
     for x in range(8): 
      coord=bytemapping[(y+1,x+1)]
      c=savebyte[coord[0]][coord[1]]
      if c:
       txt+="#"
      else:
       txt+="."
     print "       ;"+txt+";"
    for ele in savebyte:
     txt=""
     for e in ele:
      if e:
       txt+="1"
      else:
       txt+="0"
     print " .byte #%"+txt
    pygame.quit()
    sys.exit()
   if event.type==pygame.MOUSEBUTTONDOWN:
    x=event.pos[0]/(512/8)+1
    y=event.pos[1]/(384/6)+1
    coord=bytemapping[(y,x)]
    savebyte[coord[0]][coord[1]]=not savebyte[coord[0]][coord[1]]

  for y in range(6):
   for x in range(8):
    coord=bytemapping[(y+1,x+1)]
    c=savebyte[coord[0]][coord[1]]
    if not c:
     s.set_at((x,y),(128,255,0))
    else:
     s.set_at((x,y),(0,128,0))
  pygame.transform.scale(s,d.get_size(),d)
  pygame.display.flip()

if __name__=="__main__":
 main()
