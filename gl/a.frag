#version 330 core
out vec4 FragColor;
in vec2 FragCoord;



float sdCircle( in vec2 p, in float r ) 
{
    return length(p)-r;
}

void main()
{
  vec2 iResolution = vec2(640,480);
  vec2 p = (2.0*gl_FragCoord.xy-iResolution.xy)/iResolution.y;

  //   p = vec2(int (p.x * 1.0) % 1,
  // int (p.y * 1.0) % 1);

  float sx = .25;
  float sy = .25;
  p.x = p.x - sx *round(p.x / sx);
  p.y = p.y - sy*round(p.y/sy);
  float d = sdCircle(p ,0.2);

  
  // coloring
  vec3 col = (d>0.0) ? vec3(0.9,0.2,0.3) : vec3(0.65,0.85,1.0);
  col *= 1.0 - exp(-6.0*abs(d));
  col *= 0.8 + 0.2*cos(150.0*d);
  col = mix( col, vec3(1.0), 1.0-smoothstep(0.0,0.01,abs(d)) );

   FragColor = vec4(col, 1.0f);
}
