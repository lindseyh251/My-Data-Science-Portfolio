# ArcheryScorer.py
# by Lindsey Hornberger
# an archery game to count the score of hits made from clicking on the target

from graphics import *
import math
 
win = GraphWin("Archery Target", 500,500)
win.setCoords(-250, -250, 250, 250)
cen = Point(0,0)
 
msg = Text(Point(0,230), 'Click to hit the target.')
msg.setFill('white')
msg.draw(win)

def score_text():
    bullseye = Text(Point(0,0), 'x') 
    bullseye.setSize(14)
    bullseye.draw(win)


def hit_target(): 
    count = 0
    for i in range(5):
 
        p = win.getMouse()
        x = p.getX()
        y = p.getY()
        p = Circle(Point(x,y),4)
        p.setFill('gray')
        p.draw(win)
        d = x**2 + y**2
 
        string = 'You hit the target. Score is %s.'
        if d > 200**2:
            msg.setText(('You missed the target. Score is %s.' %count))
        elif d <= 48**2:
            count += 9
            msg.setText(('You hit the bulls-eye! Score is %s.' %count)) 
        elif d <= 86**2:
            count += 7
            message = msg.setText((string %count))
        elif d <= 124**2:
            count += 5
            message = msg.setText((string %count))
        elif d <= 162**2:
            count += 3
            message = msg.setText((string %count))
        elif d <= 200**2:
            count += 1
            message = msg.setText((string %count))
 
    msg.setText('Game over. Score is %s. Click anywhere to quit.' %count)
    win.getMouse()
    win.close()
 
    return count
 
def main(): 
    #draw the target
    rad = 200
    color = ['black', 'white', 'red', 'green', 'yellow']
    for i in range(5):
        for j in range(1):
            c = Circle(cen, rad)
            c.setFill(color[i]) 
            c.setWidth('2') 
            c.draw(win)
            rad = (rad - 39)
            
    #draw the bull's-eye
    center = Circle(cen,10)
    center.setWidth('2')
    score_text()
    hit_target()
 
if __name__ == '__main__': 
    main()

