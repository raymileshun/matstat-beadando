function Circle(x,y,r){
  this.body=Bodies.circle(x,y,r)


  World.add(world,this.body)

  this.show=function(){
    let pos = this.body.position;

    push()
    rectMode(CENTER)
    translate(pos.x,pos.y)
    strokeWeight(this.body.circleRadius)
    ellipse(0,0,this.body.circleRadius)
    pop()

  }


}
