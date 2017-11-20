#version 330 core

in vec4 Color;
out vec4 outColor;

void main()
{
    outColor = Color;
    //outColor = vec4(0,0,0.25, 0.25);
}
