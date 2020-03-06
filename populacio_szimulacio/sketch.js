let mean=100.5;
let sd=20.5;
let imagesNumber=10;

let sdSlider;
let meanSlider;
let offset=50;
let backGroundColor=[200]
let calculatedSizes=[];
let randomPositions=[];
let randomHeights=[];

let img;


function setup() {
  createCanvas(1200, 800);
  meanSlider = createSlider(0.1, 450, mean);
  sdSlider = createSlider(20, 250, sd);
  meanSlider.position(100,300)
  sdSlider.position(100,350)
  for(let i=0;i<imagesNumber;i++){
    randomPositions.push(random(100,220))
    randomHeights.push(random(50,700))
  }
}




function draw() {
  background(backGroundColor)

  if(mean!==meanSlider.value()||sd!==sdSlider.value()){
    mean=meanSlider.value();
    sd=sdSlider.value();
    calculateImageSizes(mean,sd);
  }
  drawGaussian(mean,sd)
  push()
  noStroke()
  fill(backGroundColor)
  rect(0,0,360,height)
  pop()
  text("Atlag: "+mean,100,290)
  text("Sd: "+sd,100,340)

  push()
  strokeWeight(10)
  line(360,height-offset,360,0)
  line(360,height-offset,width,height-offset)
  textSize(20)
  text("MÉRET",400,height-20)
  text("DARABSZÁM",220,30)
  pop()

  push()
  noStroke()
  fill(150)
  rect(width-300,0,width,height)
  pop()

  for(let i=0;i<imagesNumber;i++){
    //image(img, width-randomPositions[i], (i*height+40)/imagesNumber, calculatedSizes[i], calculatedSizes[i]);
    // ellipse(width-randomPositions[i],(i*height+40)/imagesNumber,calculatedSizes[i])
    push()
    textSize(calculatedSizes[i])
    text('🐶',width-randomPositions[i],(i*height+40)/imagesNumber)
    pop()

  }

}

function normalDistributionForAPoint(pnt, mean, sd) {
  return (1 / sqrt(TWO_PI * sd)) * exp(-sq(pnt - mean) / (2 * sd));
}

function drawGaussian( mean, variance) {
  beginShape();
  for(let i = 0; i < width; i++) {
    let x = map(i, 0, width, -5, 5);
    let y = normalDistributionForAPoint(x, (mean/100)-2, variance/200)/2;
    let screenx = map(x, -5, 5, 0, width);
    let screeny = map(y, 0, 1, height-offset, 0);
    vertex(screenx, screeny);
  }
  endShape();
}

function calculateImageSizes(mean,sd){
  calculatedSizes=[];
  for(let i=0;i<imagesNumber;i++){
    calculatedSizes.push(randomGaussian(mean/5,sd/5))
  }
}
