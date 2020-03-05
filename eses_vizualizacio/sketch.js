var Engine = Matter.Engine,
    //Render = Matter.Render,
    World = Matter.World,
    Bodies = Matter.Bodies;

var engine;
var world;
var points=[];
let ground, leftWall, rightWall;
let sdLine, sdConstrainLeft,sdConstrainRight,offset=40;

let separators=[];
let separatorCount=10;
let pointRadius=12;
let visible=true
let slider;

let urlParams =window.location.search;
let screenWidth=800, screenHeight=500;
let sdLineLength,sdLineHeightPos=100;


function setup() {

  if(urlParams.includes('screenWidth')){
    screenWidth=urlParams.split('=')[1]
  }
  sdLineLength=screenWidth/2-50
  createCanvas(screenWidth, 500);
  slider = createSlider(50, 2*sdLineLength, sdLineLength);
  rectMode(CENTER)

  engine=Engine.create()
  world=engine.world;
  Engine.run(engine)
  ground=Bodies.rectangle(width/2,height,width,20,{isStatic:true})
  leftWall=Bodies.rectangle(0,height,1,2*height,{isStatic:true})
  rightWall=Bodies.rectangle(width,height,1,2*height,{isStatic:true})
  World.add(world,[ground,leftWall,rightWall])

  addBoxToVisualisation();

  for(let i=0;i<separatorCount;i++){
    separators.push(Bodies.rectangle((0+i*width/separatorCount),height,4,height,{isStatic:true}))
    World.add(world,separators[i]);
  }
}

function addBoxToVisualisation(){


  sdLine=Bodies.rectangle((width/2),sdLineHeightPos,sdLineLength,2,{isStatic:true})
  sdConstrainLeft=Bodies.rectangle(sdLine.bounds.min.x,sdLineHeightPos/2-offset,2,sdLineHeightPos+2*offset,{isStatic:true})
  sdConstrainRight=Bodies.rectangle(sdLine.bounds.max.x,sdLineHeightPos/2-offset,2,sdLineHeightPos+2*offset,{isStatic:true})

  World.add(world,[sdLine,sdConstrainLeft,sdConstrainRight])

}


function mouseDragged(){
  if(mouseX>sdLine.bounds.min.x&&mouseX<sdLine.bounds.max.x&&mouseY<sdLineHeightPos){
  points.push(new Circle(mouseX,mouseY,pointRadius))
  }
}

function keyPressed() {
  if (keyCode === LEFT_ARROW) {
    Matter.Composite.remove(world, sdLine)
    visible=false
  }
}

function drawPointsContainer(){
  if(visible){
    push()
    fill(0)
    strokeWeight(2);
    line(sdLine.bounds.min.x,sdLine.bounds.min.y,sdLine.bounds.max.x,sdLine.bounds.max.y)
    pop()
  }

  fill(0)
  strokeWeight(2);
  line(sdConstrainLeft.bounds.min.x,sdConstrainLeft.bounds.min.y,sdConstrainLeft.bounds.max.x,sdConstrainLeft.bounds.max.y)

  fill(0)
  strokeWeight(2);
  line(sdConstrainRight.bounds.min.x,sdConstrainRight.bounds.min.y,sdConstrainRight.bounds.max.x,sdConstrainRight.bounds.max.y)
}

function draw() {
  if(sdLineLength!==slider.value()){
    Matter.Composite.remove(world, [sdLine,sdConstrainLeft,sdConstrainRight])
    sdLineLength=slider.value();
    visible=true
    addBoxToVisualisation();
  }

  background(200)
  drawPointsContainer();



  for(let i=0;i<points.length;i++){
    points[i].show();
  }

  push()
  fill(0)
  strokeWeight(15);
  line(0,height,width,height)
  pop()

  for(let i=0;i<separators.length;i++){
    line(separators[i].position.x,separators[i].position.y,separators[i].position.x,separators[i].bounds.min.y);
  }

}
