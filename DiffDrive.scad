// from https://github.com/TimPut/cutGears
use <./cutGears/gears.scad>

h = 5; // Gear height
w = 2; // Lip height
wh = 0;
clearance = 0.5; // post clearance
postSize = 5;
$fn=30;

// From output of hSlew
sunTeeth = 39;
planetTeeth = 13;
ringInner = 65;
ringOuter = 86;
carrierTeeth = 92;
ringDrive = 17;
carrierDrive = 11;
ratio = -94.19047619047666;
c2cDist = 66.95;
mod = 1.3;
ps = [[33.800000000000004,0.0],[-15.707643215079377,29.928413667078498],[-19.200588439513073,-27.81685466720559]];

Sun();
Ring();
Planets();
Carrier();

module Sun(){
  difference(){
    translate([0,0,h/2])union(){
      gear_helix(z = sunTeeth, m = mod, h = h, w_helix = wh, clearance = 0.01);
      // Bearing lip cup
      difference(){
        translate([0,0,h/2])cylinder(d=34+6,h=2); // bearing standoff diameter plus lip
        translate([0,0,h/2+1])cylinder(d=34,h=1.1);
      };
    };
    cube([18,18,18],center=true);
  };
};

module Ring(){
  union(){
    // Outer Ring
    difference(){
      translate([0,0,h/2])gear_helix(z = ringOuter, m = mod, h = h, w_helix = -wh, clearance = 0.01);
      translate([0,0,-5])cylinder(h=20,d=mod*(ringInner+2.5));
    };
    // Inner Ring
    rotate([0,0,5])translate([0,0,h/2])Gear_helix(z = ringInner, m = mod, h = h, w_helix = -wh, clearance = 0.01);
  };
};

module Planets(){
  for(p=ps){
    translate(p){    
      translate([0,0,h/2])
        difference(){
        union(){
          rotate([0,0,6])gear_helix(z = planetTeeth, m = mod, h = h, w_helix = -wh);
          translate([0,0,h/2])cylinder(d=mod*(planetTeeth+2)+5,h=2);
        };
        union(){
          cylinder(d=10,h=10);
          cylinder(d=postSize+clearance,h=100,center=true);
        }
      };
    };
  };
};

module Carrier(){
  union(){
    difference(){
      union(){
        translate([0,0,-h/2])gear_helix(z = carrierTeeth, m = mod, h = h, w_helix = wh, clearance = 0.01);
        for(p=ps){
          translate(p){
            cylinder(d=5,h=2);
          };
        };
      };
      
      union(){
        for(p=ps){
          translate(p){
            cylinder(d=2.5,h=30,center=true); // tap drill for m3
          };
        };
        cylinder(d=50,center=true,h=100); // Clearance for bearing cups
      };
    };
  };
};
