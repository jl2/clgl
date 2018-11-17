#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec4 color;

uniform mat4 transformationMatrix;

out vec4 Color;

void main()
{
    vec4 transformedPosition = transformationMatrix * vec4(position.xyz, 1.0);
    vec4 L = vec4(normalize(vec3(transformedPosition.xyz - vec3(0.0,0.0,100.0))), 1.0);

    Color = color * max(0.5, dot(L, transpose(inverse(transformationMatrix)) * vec4(normal, 0.0)));
    gl_PointSize = 1.0;
    gl_Position = transformedPosition;
}
