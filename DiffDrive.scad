// from https://github.com/TimPut/cutGears
use <./cutGears/gears.scad>

h = 5; // Gear height
w = 2; // Lip height
wh = 0;
clearance = 0.5; // post clearance
postSize = 5;
$fn=30;

// From output of hSlew
sunTeeth = 14;
planetTeeth = 6;
ringInner = 26;
ringOuter = 33;
carrierTeeth = 36;
ringDrive = 10;
carrierDrive = 7;
ratio = -138.5999999999997;
c2cDist = 86.0;
mod = 4.0;
ps = [[40.0,0.0],[-18.159619989581866,35.640260967534715],[-23.51141009169893,-32.36067977499789]];
Sun();
Ring();
Planets();
Carrier();
Drive();

module Drive(){
  translate([c2cDist,0,0]){
    difference(){
      union(){
        translate([0,0,h/2])gear_helix(z = ringDrive, m = mod, h = h, w_helix = wh, clearance = 0.01);
        translate([0,0,-h/2])gear_helix(z = carrierDrive, m = mod, h = h, w_helix = wh, clearance = 0.01);
      }
      difference(){
        cylinder(d=5+0.2,h=100,center=true);
        translate([-10,2,-50])cube([100,100,100]);
      }
    }
  }
}
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
        cylinder(d=34+6+1,center=true,h=100); // Clearance for bearing cups
      };
    };
  };
};
