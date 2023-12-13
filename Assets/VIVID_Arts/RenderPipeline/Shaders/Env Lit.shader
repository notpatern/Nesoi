// Made with Amplify Shader Editor v1.9.1.2
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "VIVID Arts/Env Lit"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin]_BaseColor("Base Color", Color) = (1,1,1,0)
		_BaseColorMap("Base Map", 2D) = "white" {}
		_NormalMap("Normal Map", 2D) = "bump" {}
		_NormalStrength("Normal Strength", Range( 0 , 2)) = 1
		_MaskMap("Mask Map", 2D) = "white" {}
		_Metallic("Metallic", Range( 0 , 1)) = 0
		_Smoothness("Smoothness", Range( 0 , 1)) = 0.5
		_OcclusionStrength("Occlusion Strength", Range( 0 , 1)) = 1
		[Toggle(_DETAIL_ON)] _Detail("Detail", Float) = 0
		_DetailMap0("Detail Map", 2D) = "gray" {}
		_DetailAlbedoStrength("Detail Albedo Strength", Range( 0 , 2)) = 1
		_DetailNormalMapScale("Detail Normal Scale", Range( 0 , 2)) = 1
		_DetailSmoothnessScale("Detail Smoothness Scale", Range( 0 , 1)) = 1
		[Toggle(_DETAIL2_ON)] _Detail2("Detail 2", Float) = 0
		_DetailMap1("Detail Map 2", 2D) = "gray" {}
		_Detail2AlbedoStrength("Detail 2 Albedo Strength", Range( 0 , 2)) = 1
		_DetailNormalMapScale2("Detail 2 Normal Scale", Range( 0 , 2)) = 1
		_Detail2SmoothnessScale("Detail 2 Smoothness Scale", Range( 0 , 1)) = 1
		[Toggle(_MOSS_ON)] _Moss("Moss", Float) = 0
		_MossColor("Moss Color", Color) = (0.6941177,0.7254902,0.2235294,0)
		_MossMap("MossMap", 2D) = "white" {}
		_MossStrength("Moss Strength", Range( 0 , 1)) = 0.2705882
		_MossNormalStrength("Moss Normal Strength", Range( 0 , 2)) = 1
		_MossSmoothness("Moss Smoothness", Range( 0 , 1)) = 0
		_MossNormalInfluence("Moss Normal Influence", Range( 0 , 1)) = 0
		_MossEdgeSoftness("Moss Edge Softness", Range( 0 , 1)) = 0.1
		_MossHeightBlendMask("Moss Height Blend Mask", Range( 0 , 1)) = 1
		_MossFresnelStrength("Moss Fresnel Strength", Float) = 0
		_MossViewFresnel("Moss View Fresnel", Vector) = (0,2.86,2,0)
		_MossAxis("Moss Axis", Vector) = (0,1,0,0)
		[Toggle(_SNOW_ON)] _Snow("Snow", Float) = 0
		_SnowColor("Snow Color", Color) = (1,1,1,0)
		_SnowMap("Snow Map", 2D) = "white" {}
		_SnowNormalStrength("Snow Normal Strength", Range( 0 , 2)) = 1
		_SnowMultiplier("Snow Multiplier", Range( 0 , 5)) = 1
		_SnowBaseNormal("Snow Base Normal", Range( 0 , 1)) = 0.75
		_SnowMaskNormalInfluence("Snow Mask Normal Influence", Range( 0 , 1)) = 0.75
		_SnowOcclusionMask("Snow Occlusion Mask", Range( 0 , 1)) = 1
		_SnowEdgeDetail("Snow Edge Detail", Range( 0 , 1)) = 0
		_SnowExposureMask("Snow Exposure Mask", Range( 0 , 1)) = 1
		_SnowHeightBlendMask("Snow Height Blend Mask", Range( 0 , 1)) = 1
		_HeightBlendOffset("Height Blend Offset", Float) = 0
		_HeightBlendColor("Height Blend Color", Color) = (0.5450981,0.6039216,0.5176471,0)
		_HeightBlendSmoothness("Height Blend Smoothness", Range( 0 , 1)) = 0.35
		_AlphaMask_Moss("Moss Mask Alpha Influence", Float) = 1
		[ASEEnd]_SnowAxis("Snow Axis", Vector) = (0,1,0,0)
		[HideInInspector] _texcoord( "", 2D ) = "white" {}

		//_TransmissionShadow( "Transmission Shadow", Range( 0, 1 ) ) = 0.5
		//_TransStrength( "Trans Strength", Range( 0, 50 ) ) = 1
		//_TransNormal( "Trans Normal Distortion", Range( 0, 1 ) ) = 0.5
		//_TransScattering( "Trans Scattering", Range( 1, 50 ) ) = 2
		//_TransDirect( "Trans Direct", Range( 0, 1 ) ) = 0.9
		//_TransAmbient( "Trans Ambient", Range( 0, 1 ) ) = 0.1
		//_TransShadow( "Trans Shadow", Range( 0, 1 ) ) = 0.5
		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" }
		Cull Back
		AlphaToMask Off
		
		HLSLINCLUDE
		#pragma target 2.0

		#pragma prefer_hlslcc gles
		

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}
		
		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS

		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 100801

			
			#pragma multi_compile _ _SCREEN_SPACE_OCCLUSION
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS _ADDITIONAL_OFF
			#pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			
			#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			#pragma multi_compile _ SHADOWS_SHADOWMASK

			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_FORWARD

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#define ASE_NEEDS_FRAG_WORLD_VIEW_DIR
			#define ASE_NEEDS_FRAG_WORLD_NORMAL
			#define ASE_NEEDS_FRAG_WORLD_TANGENT
			#define ASE_NEEDS_FRAG_WORLD_BITANGENT
			#define ASE_NEEDS_FRAG_COLOR
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#pragma shader_feature_local _SNOW_ON
			#pragma shader_feature_local _MOSS_ON
			#pragma shader_feature_local _DETAIL2_ON
			#pragma shader_feature_local _DETAIL_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord : TEXCOORD0;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _SnowColor;
			float4 _NormalMap_ST;
			float4 _SnowMap_ST;
			float4 _MossAxis;
			float4 _MossMap_ST;
			float4 _MossColor;
			float4 _DetailMap0_ST;
			float4 _SnowAxis;
			float4 _DetailMap1_ST;
			float4 _BaseColorMap_ST;
			float4 _MaskMap_ST;
			float4 _HeightBlendColor;
			float3 _MossViewFresnel;
			float _SnowHeightBlendMask;
			float _HeightBlendOffset;
			float _MossHeightBlendMask;
			float _AlphaMask_Moss;
			float _Metallic;
			float _SnowBaseNormal;
			float _SnowOcclusionMask;
			float _Smoothness;
			float _Detail2SmoothnessScale;
			float _DetailSmoothnessScale;
			float _MossNormalStrength;
			float _SnowEdgeDetail;
			float _SnowMaskNormalInfluence;
			float _SnowMultiplier;
			float _SnowNormalStrength;
			float _MossSmoothness;
			float _OcclusionStrength;
			float _MossEdgeSoftness;
			float _MossStrength;
			float _MossNormalInfluence;
			float _DetailNormalMapScale;
			float _DetailNormalMapScale2;
			float _NormalStrength;
			float _MossFresnelStrength;
			float _DetailAlbedoStrength;
			float _Detail2AlbedoStrength;
			float _SnowExposureMask;
			float _HeightBlendSmoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _BaseColorMap;
			sampler2D _DetailMap1;
			sampler2D _DetailMap0;
			sampler2D _MaskMap;
			sampler2D _MossMap;
			sampler2D _NormalMap;
			sampler2D _SnowMap;
			float GlobalSnowStrength;
			float GlobalHeightBlend;
			float SnowSmoothness;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord7.xy = v.texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					o.lightmapUVOrVertexSH.zw = v.texcoord;
					o.lightmapUVOrVertexSH.xy = v.texcoord * unity_LightmapST.xy + unity_LightmapST.zw;
				#endif

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
					half fogFactor = ComputeFogFactor( positionCS.z );
				#else
					half fogFactor = 0;
				#endif
				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif

			half4 frag ( VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#endif
	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float2 uv_BaseColorMap = IN.ase_texcoord7.xy * _BaseColorMap_ST.xy + _BaseColorMap_ST.zw;
				float4 tex2DNode10 = tex2D( _BaseColorMap, uv_BaseColorMap );
				float4 temp_output_16_0 = ( float4( 1,1,1,0 ) * ( _BaseColor * tex2DNode10 ) );
				float4 appendResult200 = (float4(0.5 , 0.5 , 0.0 , 0.5));
				float4 Default188 = appendResult200;
				float2 uv_DetailMap1 = IN.ase_texcoord7.xy * _DetailMap1_ST.xy + _DetailMap1_ST.zw;
				#ifdef _DETAIL2_ON
				float4 staticSwitch191 = tex2D( _DetailMap1, uv_DetailMap1 );
				#else
				float4 staticSwitch191 = Default188;
				#endif
				float4 break202 = staticSwitch191;
				float lerpResult628 = lerp( 0.5 , break202.r , _Detail2AlbedoStrength);
				float DetailMap2_Albedo59 = lerpResult628;
				float2 uv_DetailMap0 = IN.ase_texcoord7.xy * _DetailMap0_ST.xy + _DetailMap0_ST.zw;
				#ifdef _DETAIL_ON
				float4 staticSwitch186 = tex2D( _DetailMap0, uv_DetailMap0 );
				#else
				float4 staticSwitch186 = Default188;
				#endif
				float4 break192 = staticSwitch186;
				float lerpResult626 = lerp( 0.5 , break192.r , _DetailAlbedoStrength);
				float DetailMap_Albedo53 = lerpResult626;
				float2 uv_MaskMap = IN.ase_texcoord7.xy * _MaskMap_ST.xy + _MaskMap_ST.zw;
				float4 tex2DNode14 = tex2D( _MaskMap, uv_MaskMap );
				float MaskmapDetailMask21 = tex2DNode14.b;
				float lerpResult168 = lerp( ( DetailMap2_Albedo59 * 2.0 ) , ( DetailMap_Albedo53 * 2.0 ) , MaskmapDetailMask21);
				float4 temp_output_159_0 = ( temp_output_16_0 * lerpResult168 );
				float2 uv_MossMap = IN.ase_texcoord7.xy * _MossMap_ST.xy + _MossMap_ST.zw;
				float4 tex2DNode205 = tex2D( _MossMap, uv_MossMap );
				float4 break246 = tex2DNode205;
				float MossMap_Albedo206 = break246.r;
				float4 temp_output_270_0 = ( MossMap_Albedo206 * _MossColor );
				float fresnelNdotV319 = dot( normalize( WorldNormal ), WorldViewDirection );
				float fresnelNode319 = ( 0.0 + _MossViewFresnel.y * pow( max( 1.0 - fresnelNdotV319 , 0.0001 ), _MossViewFresnel.z ) );
				float MaskmapOcclusion19 = tex2DNode14.g;
				float3 unpack214 = UnpackNormalScale( float4( 0,0,0,0 ), 0.0 );
				unpack214.z = lerp( 1, unpack214.z, saturate(0.0) );
				float2 uv_NormalMap = IN.ase_texcoord7.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;
				float3 unpack42 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalStrength );
				unpack42.z = lerp( 1, unpack42.z, saturate(_NormalStrength) );
				float4 appendResult60 = (float4(break202.a , break202.g , 1.0 , 1.0));
				float3 unpack64 = UnpackNormalScale( appendResult60, _DetailNormalMapScale2 );
				unpack64.z = lerp( 1, unpack64.z, saturate(_DetailNormalMapScale2) );
				float3 DetailMap2_Normal66 = unpack64;
				float4 appendResult52 = (float4(break192.a , break192.g , 1.0 , 1.0));
				float3 unpack56 = UnpackNormalScale( appendResult52, _DetailNormalMapScale );
				unpack56.z = lerp( 1, unpack56.z, saturate(_DetailNormalMapScale) );
				float3 DetailMap_Normal51 = unpack56;
				float3 lerpResult81 = lerp( DetailMap2_Normal66 , DetailMap_Normal51 , MaskmapDetailMask21);
				float3 temp_output_55_0 = BlendNormal( unpack42 , lerpResult81 );
				float3 NormalBase287 = temp_output_55_0;
				float3 lerpResult215 = lerp( unpack214 , NormalBase287 , _MossNormalInfluence);
				float3 tanToWorld0 = float3( WorldTangent.x, WorldBiTangent.x, WorldNormal.x );
				float3 tanToWorld1 = float3( WorldTangent.y, WorldBiTangent.y, WorldNormal.y );
				float3 tanToWorld2 = float3( WorldTangent.z, WorldBiTangent.z, WorldNormal.z );
				float3 tanNormal216 = lerpResult215;
				float3 worldNormal216 = float3(dot(tanToWorld0,tanNormal216), dot(tanToWorld1,tanNormal216), dot(tanToWorld2,tanNormal216));
				float temp_output_348_0 = ( 1.0 - _MossStrength );
				float lerpResult140 = lerp( 1.0 , ( MaskmapOcclusion19 * IN.ase_color.g ) , _OcclusionStrength);
				float4 lerpResult293 = lerp( float4(0,0,1,0) , float4( NormalBase287 , 0.0 ) , _SnowMaskNormalInfluence);
				float2 uv_SnowMap = IN.ase_texcoord7.xy * _SnowMap_ST.xy + _SnowMap_ST.zw;
				float4 break236 = tex2D( _SnowMap, uv_SnowMap );
				float4 appendResult238 = (float4(break236.a , break236.g , 1.0 , 1.0));
				float3 unpack240 = UnpackNormalScale( appendResult238, _SnowNormalStrength );
				unpack240.z = lerp( 1, unpack240.z, saturate(_SnowNormalStrength) );
				#ifdef _SNOW_ON
				float4 staticSwitch235 = float4( unpack240 , 0.0 );
				#else
				float4 staticSwitch235 = float4(0,0,1,0);
				#endif
				float4 SnowMap_Normal241 = staticSwitch235;
				float saferPower249 = abs( WorldNormal.y );
				float4 lerpResult253 = lerp( lerpResult293 , SnowMap_Normal241 , saturate( (0.0 + (pow( saferPower249 , 3.0 ) - 0.0) * (1.0 - 0.0) / (1.0 - 0.0)) ));
				float3 tanNormal399 = lerpResult253.rgb;
				float3 worldNormal399 = float3(dot(tanToWorld0,tanNormal399), dot(tanToWorld1,tanNormal399), dot(tanToWorld2,tanNormal399));
				float temp_output_461_0 = ( GlobalSnowStrength * _SnowMultiplier );
				float ExposureMask375 = IN.ase_color.b;
				float lerpResult382 = lerp( ExposureMask375 , 1.0 , _SnowExposureMask);
				float SnowMap_Albedo237 = break236.r;
				float lerpResult372 = lerp( 1.0 , saturate( (1.0 + (( (1.0 + (( MaskmapOcclusion19 * lerpResult382 ) - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) / saturate( (0.0 + (SnowMap_Albedo237 - _SnowEdgeDetail) * (1.0 - 0.0) / (1.0 - _SnowEdgeDetail)) ) ) - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) ) , _SnowOcclusionMask);
				float temp_output_93_0 = ( GlobalHeightBlend + _HeightBlendOffset );
				float HeightBlendMask96 = saturate( (temp_output_93_0 + (-WorldPosition.y - 0.0) * (( temp_output_93_0 + 1.0 ) - temp_output_93_0) / (1.0 - 0.0)) );
				float lerpResult309 = lerp( ( saturate( (-4.1 + (saturate( ( saturate( ( worldNormal399.x * _SnowAxis.x ) ) + saturate( ( worldNormal399.y * _SnowAxis.y ) ) + saturate( ( worldNormal399.z * _SnowAxis.z ) ) ) ) - ( 1.0 - temp_output_461_0 )) * (( 1.9 * temp_output_461_0 ) - -4.1) / (1.0 - ( 1.0 - temp_output_461_0 ))) ) * lerpResult372 ) , 0.0 , ( _SnowHeightBlendMask * HeightBlendMask96 ));
				float SnowMask260 = lerpResult309;
				float lerpResult587 = lerp( lerpResult140 , 1.0 , SnowMask260);
				float OcclusionOutput28 = lerpResult587;
				float lerpResult329 = lerp( saturate( ( saturate( (0.0 + (( saturate( ( worldNormal216.x * _MossAxis.x ) ) + saturate( ( worldNormal216.y * _MossAxis.y ) ) + saturate( ( worldNormal216.z * _MossAxis.z ) ) + _MossAxis.w ) - temp_output_348_0) * (( 2.0 * _MossStrength ) - 0.0) / (( temp_output_348_0 + _MossEdgeSoftness ) - temp_output_348_0)) ) * OcclusionOutput28 ) ) , 0.0 , ( _MossHeightBlendMask * HeightBlendMask96 ));
				float AlphaMask342 = tex2DNode10.a;
				float lerpResult467 = lerp( 1.0 , AlphaMask342 , _AlphaMask_Moss);
				float lerpResult474 = lerp( (1.0 + (AlphaMask342 - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) , 1.0 , ( _AlphaMask_Moss - -1.0 ));
				float temp_output_507_0 = ( saturate( lerpResult467 ) * saturate( lerpResult474 ) );
				float temp_output_462_0 = ( lerpResult329 * temp_output_507_0 );
				float MossMask227 = temp_output_462_0;
				float4 lerpResult272 = lerp( temp_output_159_0 , ( temp_output_270_0 + ( ( temp_output_270_0 * ( _MossColor * ( fresnelNode319 * _MossFresnelStrength ) ) ) * MaskmapOcclusion19 ) ) , MossMask227);
				#ifdef _MOSS_ON
				float4 staticSwitch280 = lerpResult272;
				#else
				float4 staticSwitch280 = temp_output_159_0;
				#endif
				float4 lerpResult99 = lerp( staticSwitch280 , ( staticSwitch280 * _HeightBlendColor ) , HeightBlendMask96);
				float4 lerpResult261 = lerp( lerpResult99 , ( _SnowColor * SnowMap_Albedo237 ) , SnowMask260);
				#ifdef _SNOW_ON
				float4 staticSwitch281 = lerpResult261;
				#else
				float4 staticSwitch281 = lerpResult99;
				#endif
				float4 AlbedoOutput39 = staticSwitch281;
				
				float4 appendResult208 = (float4(break246.a , break246.g , 1.0 , 1.0));
				float3 unpack209 = UnpackNormalScale( appendResult208, _MossNormalStrength );
				unpack209.z = lerp( 1, unpack209.z, saturate(_MossNormalStrength) );
				float3 MossMap_Normal210 = unpack209;
				float3 lerpResult330 = lerp( temp_output_55_0 , MossMap_Normal210 , MossMask227);
				#ifdef _MOSS_ON
				float3 staticSwitch333 = lerpResult330;
				#else
				float3 staticSwitch333 = temp_output_55_0;
				#endif
				float4 lerpResult290 = lerp( SnowMap_Normal241 , float4( BlendNormal( staticSwitch333 , SnowMap_Normal241.rgb ) , 0.0 ) , _SnowBaseNormal);
				float4 lerpResult285 = lerp( float4( staticSwitch333 , 0.0 ) , lerpResult290 , SnowMask260);
				#ifdef _SNOW_ON
				float4 staticSwitch334 = lerpResult285;
				#else
				float4 staticSwitch334 = float4( staticSwitch333 , 0.0 );
				#endif
				float4 NormalOutput67 = staticSwitch334;
				
				float MaskmapMetallic22 = tex2DNode14.r;
				float MetallicOutput35 = ( MaskmapMetallic22 * _Metallic );
				
				float MaskmapSmoothness20 = tex2DNode14.a;
				float DetailMap2_Smoothness65 = ( break202.b * _Detail2SmoothnessScale );
				float DetailMap_Smoothness54 = ( break192.b * _DetailSmoothnessScale );
				float lerpResult630 = lerp( ( DetailMap2_Smoothness65 * 2.0 ) , ( DetailMap_Smoothness54 * 2.0 ) , MaskmapDetailMask21);
				float temp_output_158_0 = ( ( MaskmapSmoothness20 * _Smoothness ) + lerpResult630 );
				float MossMap_Smoothness207 = break246.b;
				float lerpResult339 = lerp( temp_output_158_0 , ( MossMap_Smoothness207 * _MossSmoothness ) , MossMask227);
				#ifdef _MOSS_ON
				float staticSwitch341 = lerpResult339;
				#else
				float staticSwitch341 = temp_output_158_0;
				#endif
				float SnowMap_Smoothness243 = break236.b;
				float lerpResult300 = lerp( staticSwitch341 , ( SnowMap_Smoothness243 * SnowSmoothness ) , SnowMask260);
				#ifdef _SNOW_ON
				float staticSwitch340 = lerpResult300;
				#else
				float staticSwitch340 = staticSwitch341;
				#endif
				float lerpResult136 = lerp( staticSwitch340 , ( staticSwitch340 + _HeightBlendSmoothness ) , HeightBlendMask96);
				float SmoothnessOutput31 = lerpResult136;
				
				float3 Albedo = AlbedoOutput39.rgb;
				float3 Normal = NormalOutput67.rgb;
				float3 Emission = 0;
				float3 Specular = 0.5;
				float Metallic = MetallicOutput35;
				float Smoothness = SmoothnessOutput31;
				float Occlusion = OcclusionOutput28;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;
				float3 BakedGI = 0;
				float3 RefractionColor = 1;
				float RefractionIndex = 1;
				float3 Transmission = 1;
				float3 Translucency = 1;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
					inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				#else
					inputData.normalWS = WorldNormal;
				#endif

				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
				#endif
				
				inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(IN.clipPos);
				inputData.shadowMask = SAMPLE_SHADOWMASK(IN.lightmapUVOrVertexSH.xy);

				half4 color = UniversalFragmentPBR(
					inputData, 
					Albedo, 
					Metallic, 
					Specular, 
					Smoothness, 
					Occlusion, 
					Emission, 
					Alpha);

				#ifdef _TRANSMISSION_ASE
				{
					float shadow = _TransmissionShadow;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
					half3 mainTransmission = max(0 , -dot(inputData.normalWS, mainLight.direction)) * mainAtten * Transmission;
					color.rgb += Albedo * mainTransmission;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 transmission = max(0 , -dot(inputData.normalWS, light.direction)) * atten * Transmission;
							color.rgb += Albedo * transmission;
						}
					#endif
				}
				#endif

				#ifdef _TRANSLUCENCY_ASE
				{
					float shadow = _TransShadow;
					float normal = _TransNormal;
					float scattering = _TransScattering;
					float direct = _TransDirect;
					float ambient = _TransAmbient;
					float strength = _TransStrength;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );

					half3 mainLightDir = mainLight.direction + inputData.normalWS * normal;
					half mainVdotL = pow( saturate( dot( inputData.viewDirectionWS, -mainLightDir ) ), scattering );
					half3 mainTranslucency = mainAtten * ( mainVdotL * direct + inputData.bakedGI * ambient ) * Translucency;
					color.rgb += Albedo * mainTranslucency * strength;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 lightDir = light.direction + inputData.normalWS * normal;
							half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );
							half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;
							color.rgb += Albedo * translucency * strength;
						}
					#endif
				}
				#endif

				#ifdef _REFRACTION_ASE
					float4 projScreenPos = ScreenPos / ScreenPos.w;
					float3 refractionOffset = ( RefractionIndex - 1.0 ) * mul( UNITY_MATRIX_V, float4( WorldNormal,0 ) ).xyz * ( 1.0 - dot( WorldNormal, WorldViewDirection ) );
					projScreenPos.xy += refractionOffset.xy;
					float3 refraction = SHADERGRAPH_SAMPLE_SCENE_COLOR( projScreenPos.xy ) * RefractionColor;
					color.rgb = lerp( refraction, color.rgb, color.a );
					color.a = 1;
				#endif

				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif

				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif

				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif

				return color;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual
			AlphaToMask Off
			ColorMask 0

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 100801

			
			#pragma vertex vert
			#pragma fragment frag
#if ASE_SRP_VERSION >= 110000
			#pragma multi_compile _ _CASTING_PUNCTUAL_LIGHT_SHADOW
#endif
			#define SHADERPASS_SHADOWCASTER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _SnowColor;
			float4 _NormalMap_ST;
			float4 _SnowMap_ST;
			float4 _MossAxis;
			float4 _MossMap_ST;
			float4 _MossColor;
			float4 _DetailMap0_ST;
			float4 _SnowAxis;
			float4 _DetailMap1_ST;
			float4 _BaseColorMap_ST;
			float4 _MaskMap_ST;
			float4 _HeightBlendColor;
			float3 _MossViewFresnel;
			float _SnowHeightBlendMask;
			float _HeightBlendOffset;
			float _MossHeightBlendMask;
			float _AlphaMask_Moss;
			float _Metallic;
			float _SnowBaseNormal;
			float _SnowOcclusionMask;
			float _Smoothness;
			float _Detail2SmoothnessScale;
			float _DetailSmoothnessScale;
			float _MossNormalStrength;
			float _SnowEdgeDetail;
			float _SnowMaskNormalInfluence;
			float _SnowMultiplier;
			float _SnowNormalStrength;
			float _MossSmoothness;
			float _OcclusionStrength;
			float _MossEdgeSoftness;
			float _MossStrength;
			float _MossNormalInfluence;
			float _DetailNormalMapScale;
			float _DetailNormalMapScale2;
			float _NormalStrength;
			float _MossFresnelStrength;
			float _DetailAlbedoStrength;
			float _Detail2AlbedoStrength;
			float _SnowExposureMask;
			float _HeightBlendSmoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			

			
			float3 _LightDirection;
#if ASE_SRP_VERSION >= 110000 
			float3 _LightPosition;
#endif
			VertexOutput VertexFunction( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif
				float3 normalWS = TransformObjectToWorldDir(v.ase_normal);

		#if ASE_SRP_VERSION >= 110000 
			#if _CASTING_PUNCTUAL_LIGHT_SHADOW
				float3 lightDirectionWS = normalize(_LightPosition - positionWS);
			#else
				float3 lightDirectionWS = _LightDirection;
			#endif
				float4 clipPos = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));
			#if UNITY_REVERSED_Z
				clipPos.z = min(clipPos.z, UNITY_NEAR_CLIP_VALUE);
			#else
				clipPos.z = max(clipPos.z, UNITY_NEAR_CLIP_VALUE);
			#endif
		#else
				float4 clipPos = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, _LightDirection));
			#if UNITY_REVERSED_Z
				clipPos.z = min(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
			#else
				clipPos.z = max(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
			#endif
		#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = clipPos;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif

			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );
				
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					#ifdef _ALPHATEST_SHADOW_ON
						clip(Alpha - AlphaClipThresholdShadow);
					#else
						clip(Alpha - AlphaClipThreshold);
					#endif
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif
				return 0;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 100801

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _SnowColor;
			float4 _NormalMap_ST;
			float4 _SnowMap_ST;
			float4 _MossAxis;
			float4 _MossMap_ST;
			float4 _MossColor;
			float4 _DetailMap0_ST;
			float4 _SnowAxis;
			float4 _DetailMap1_ST;
			float4 _BaseColorMap_ST;
			float4 _MaskMap_ST;
			float4 _HeightBlendColor;
			float3 _MossViewFresnel;
			float _SnowHeightBlendMask;
			float _HeightBlendOffset;
			float _MossHeightBlendMask;
			float _AlphaMask_Moss;
			float _Metallic;
			float _SnowBaseNormal;
			float _SnowOcclusionMask;
			float _Smoothness;
			float _Detail2SmoothnessScale;
			float _DetailSmoothnessScale;
			float _MossNormalStrength;
			float _SnowEdgeDetail;
			float _SnowMaskNormalInfluence;
			float _SnowMultiplier;
			float _SnowNormalStrength;
			float _MossSmoothness;
			float _OcclusionStrength;
			float _MossEdgeSoftness;
			float _MossStrength;
			float _MossNormalInfluence;
			float _DetailNormalMapScale;
			float _DetailNormalMapScale2;
			float _NormalStrength;
			float _MossFresnelStrength;
			float _DetailAlbedoStrength;
			float _Detail2AlbedoStrength;
			float _SnowExposureMask;
			float _HeightBlendSmoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			

			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				#ifdef ASE_DEPTH_WRITE_ON
				outputDepth = DepthValue;
				#endif

				return 0;
			}
			ENDHLSL
		}
		
		
		Pass
		{
			
			Name "Meta"
			Tags { "LightMode"="Meta" }

			Cull Off

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 100801

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _SNOW_ON
			#pragma shader_feature_local _MOSS_ON
			#pragma shader_feature_local _DETAIL2_ON
			#pragma shader_feature_local _DETAIL_ON


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _SnowColor;
			float4 _NormalMap_ST;
			float4 _SnowMap_ST;
			float4 _MossAxis;
			float4 _MossMap_ST;
			float4 _MossColor;
			float4 _DetailMap0_ST;
			float4 _SnowAxis;
			float4 _DetailMap1_ST;
			float4 _BaseColorMap_ST;
			float4 _MaskMap_ST;
			float4 _HeightBlendColor;
			float3 _MossViewFresnel;
			float _SnowHeightBlendMask;
			float _HeightBlendOffset;
			float _MossHeightBlendMask;
			float _AlphaMask_Moss;
			float _Metallic;
			float _SnowBaseNormal;
			float _SnowOcclusionMask;
			float _Smoothness;
			float _Detail2SmoothnessScale;
			float _DetailSmoothnessScale;
			float _MossNormalStrength;
			float _SnowEdgeDetail;
			float _SnowMaskNormalInfluence;
			float _SnowMultiplier;
			float _SnowNormalStrength;
			float _MossSmoothness;
			float _OcclusionStrength;
			float _MossEdgeSoftness;
			float _MossStrength;
			float _MossNormalInfluence;
			float _DetailNormalMapScale;
			float _DetailNormalMapScale2;
			float _NormalStrength;
			float _MossFresnelStrength;
			float _DetailAlbedoStrength;
			float _Detail2AlbedoStrength;
			float _SnowExposureMask;
			float _HeightBlendSmoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _BaseColorMap;
			sampler2D _DetailMap1;
			sampler2D _DetailMap0;
			sampler2D _MaskMap;
			sampler2D _MossMap;
			sampler2D _NormalMap;
			sampler2D _SnowMap;
			float GlobalSnowStrength;
			float GlobalHeightBlend;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float3 normalizeWorldNormal = normalize( TransformObjectToWorldNormal(v.ase_normal) );
				o.ase_texcoord3.xyz = normalizeWorldNormal;
				float3 ase_worldTangent = TransformObjectToWorldDir(v.ase_tangent.xyz);
				o.ase_texcoord4.xyz = ase_worldTangent;
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord5.xyz = ase_worldNormal;
				float ase_vertexTangentSign = v.ase_tangent.w * ( unity_WorldTransformParams.w >= 0.0 ? 1.0 : -1.0 );
				float3 ase_worldBitangent = cross( ase_worldNormal, ase_worldTangent ) * ase_vertexTangentSign;
				o.ase_texcoord6.xyz = ase_worldBitangent;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				o.ase_texcoord3.w = 0;
				o.ase_texcoord4.w = 0;
				o.ase_texcoord5.w = 0;
				o.ase_texcoord6.w = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = MetaVertexPosition( v.vertex, v.texcoord1.xy, v.texcoord1.xy, unity_LightmapST, unity_DynamicLightmapST );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.texcoord1 = v.texcoord1;
				o.texcoord2 = v.texcoord2;
				o.ase_texcoord = v.ase_texcoord;
				o.ase_tangent = v.ase_tangent;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.texcoord2 = patch[0].texcoord2 * bary.x + patch[1].texcoord2 * bary.y + patch[2].texcoord2 * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_BaseColorMap = IN.ase_texcoord2.xy * _BaseColorMap_ST.xy + _BaseColorMap_ST.zw;
				float4 tex2DNode10 = tex2D( _BaseColorMap, uv_BaseColorMap );
				float4 temp_output_16_0 = ( float4( 1,1,1,0 ) * ( _BaseColor * tex2DNode10 ) );
				float4 appendResult200 = (float4(0.5 , 0.5 , 0.0 , 0.5));
				float4 Default188 = appendResult200;
				float2 uv_DetailMap1 = IN.ase_texcoord2.xy * _DetailMap1_ST.xy + _DetailMap1_ST.zw;
				#ifdef _DETAIL2_ON
				float4 staticSwitch191 = tex2D( _DetailMap1, uv_DetailMap1 );
				#else
				float4 staticSwitch191 = Default188;
				#endif
				float4 break202 = staticSwitch191;
				float lerpResult628 = lerp( 0.5 , break202.r , _Detail2AlbedoStrength);
				float DetailMap2_Albedo59 = lerpResult628;
				float2 uv_DetailMap0 = IN.ase_texcoord2.xy * _DetailMap0_ST.xy + _DetailMap0_ST.zw;
				#ifdef _DETAIL_ON
				float4 staticSwitch186 = tex2D( _DetailMap0, uv_DetailMap0 );
				#else
				float4 staticSwitch186 = Default188;
				#endif
				float4 break192 = staticSwitch186;
				float lerpResult626 = lerp( 0.5 , break192.r , _DetailAlbedoStrength);
				float DetailMap_Albedo53 = lerpResult626;
				float2 uv_MaskMap = IN.ase_texcoord2.xy * _MaskMap_ST.xy + _MaskMap_ST.zw;
				float4 tex2DNode14 = tex2D( _MaskMap, uv_MaskMap );
				float MaskmapDetailMask21 = tex2DNode14.b;
				float lerpResult168 = lerp( ( DetailMap2_Albedo59 * 2.0 ) , ( DetailMap_Albedo53 * 2.0 ) , MaskmapDetailMask21);
				float4 temp_output_159_0 = ( temp_output_16_0 * lerpResult168 );
				float2 uv_MossMap = IN.ase_texcoord2.xy * _MossMap_ST.xy + _MossMap_ST.zw;
				float4 tex2DNode205 = tex2D( _MossMap, uv_MossMap );
				float4 break246 = tex2DNode205;
				float MossMap_Albedo206 = break246.r;
				float4 temp_output_270_0 = ( MossMap_Albedo206 * _MossColor );
				float3 ase_worldViewDir = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				ase_worldViewDir = normalize(ase_worldViewDir);
				float3 normalizeWorldNormal = IN.ase_texcoord3.xyz;
				float fresnelNdotV319 = dot( normalizeWorldNormal, ase_worldViewDir );
				float fresnelNode319 = ( 0.0 + _MossViewFresnel.y * pow( max( 1.0 - fresnelNdotV319 , 0.0001 ), _MossViewFresnel.z ) );
				float MaskmapOcclusion19 = tex2DNode14.g;
				float3 unpack214 = UnpackNormalScale( float4( 0,0,0,0 ), 0.0 );
				unpack214.z = lerp( 1, unpack214.z, saturate(0.0) );
				float2 uv_NormalMap = IN.ase_texcoord2.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;
				float3 unpack42 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalStrength );
				unpack42.z = lerp( 1, unpack42.z, saturate(_NormalStrength) );
				float4 appendResult60 = (float4(break202.a , break202.g , 1.0 , 1.0));
				float3 unpack64 = UnpackNormalScale( appendResult60, _DetailNormalMapScale2 );
				unpack64.z = lerp( 1, unpack64.z, saturate(_DetailNormalMapScale2) );
				float3 DetailMap2_Normal66 = unpack64;
				float4 appendResult52 = (float4(break192.a , break192.g , 1.0 , 1.0));
				float3 unpack56 = UnpackNormalScale( appendResult52, _DetailNormalMapScale );
				unpack56.z = lerp( 1, unpack56.z, saturate(_DetailNormalMapScale) );
				float3 DetailMap_Normal51 = unpack56;
				float3 lerpResult81 = lerp( DetailMap2_Normal66 , DetailMap_Normal51 , MaskmapDetailMask21);
				float3 temp_output_55_0 = BlendNormal( unpack42 , lerpResult81 );
				float3 NormalBase287 = temp_output_55_0;
				float3 lerpResult215 = lerp( unpack214 , NormalBase287 , _MossNormalInfluence);
				float3 ase_worldTangent = IN.ase_texcoord4.xyz;
				float3 ase_worldNormal = IN.ase_texcoord5.xyz;
				float3 ase_worldBitangent = IN.ase_texcoord6.xyz;
				float3 tanToWorld0 = float3( ase_worldTangent.x, ase_worldBitangent.x, ase_worldNormal.x );
				float3 tanToWorld1 = float3( ase_worldTangent.y, ase_worldBitangent.y, ase_worldNormal.y );
				float3 tanToWorld2 = float3( ase_worldTangent.z, ase_worldBitangent.z, ase_worldNormal.z );
				float3 tanNormal216 = lerpResult215;
				float3 worldNormal216 = float3(dot(tanToWorld0,tanNormal216), dot(tanToWorld1,tanNormal216), dot(tanToWorld2,tanNormal216));
				float temp_output_348_0 = ( 1.0 - _MossStrength );
				float lerpResult140 = lerp( 1.0 , ( MaskmapOcclusion19 * IN.ase_color.g ) , _OcclusionStrength);
				float4 lerpResult293 = lerp( float4(0,0,1,0) , float4( NormalBase287 , 0.0 ) , _SnowMaskNormalInfluence);
				float2 uv_SnowMap = IN.ase_texcoord2.xy * _SnowMap_ST.xy + _SnowMap_ST.zw;
				float4 break236 = tex2D( _SnowMap, uv_SnowMap );
				float4 appendResult238 = (float4(break236.a , break236.g , 1.0 , 1.0));
				float3 unpack240 = UnpackNormalScale( appendResult238, _SnowNormalStrength );
				unpack240.z = lerp( 1, unpack240.z, saturate(_SnowNormalStrength) );
				#ifdef _SNOW_ON
				float4 staticSwitch235 = float4( unpack240 , 0.0 );
				#else
				float4 staticSwitch235 = float4(0,0,1,0);
				#endif
				float4 SnowMap_Normal241 = staticSwitch235;
				float saferPower249 = abs( ase_worldNormal.y );
				float4 lerpResult253 = lerp( lerpResult293 , SnowMap_Normal241 , saturate( (0.0 + (pow( saferPower249 , 3.0 ) - 0.0) * (1.0 - 0.0) / (1.0 - 0.0)) ));
				float3 tanNormal399 = lerpResult253.rgb;
				float3 worldNormal399 = float3(dot(tanToWorld0,tanNormal399), dot(tanToWorld1,tanNormal399), dot(tanToWorld2,tanNormal399));
				float temp_output_461_0 = ( GlobalSnowStrength * _SnowMultiplier );
				float ExposureMask375 = IN.ase_color.b;
				float lerpResult382 = lerp( ExposureMask375 , 1.0 , _SnowExposureMask);
				float SnowMap_Albedo237 = break236.r;
				float lerpResult372 = lerp( 1.0 , saturate( (1.0 + (( (1.0 + (( MaskmapOcclusion19 * lerpResult382 ) - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) / saturate( (0.0 + (SnowMap_Albedo237 - _SnowEdgeDetail) * (1.0 - 0.0) / (1.0 - _SnowEdgeDetail)) ) ) - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) ) , _SnowOcclusionMask);
				float temp_output_93_0 = ( GlobalHeightBlend + _HeightBlendOffset );
				float HeightBlendMask96 = saturate( (temp_output_93_0 + (-WorldPosition.y - 0.0) * (( temp_output_93_0 + 1.0 ) - temp_output_93_0) / (1.0 - 0.0)) );
				float lerpResult309 = lerp( ( saturate( (-4.1 + (saturate( ( saturate( ( worldNormal399.x * _SnowAxis.x ) ) + saturate( ( worldNormal399.y * _SnowAxis.y ) ) + saturate( ( worldNormal399.z * _SnowAxis.z ) ) ) ) - ( 1.0 - temp_output_461_0 )) * (( 1.9 * temp_output_461_0 ) - -4.1) / (1.0 - ( 1.0 - temp_output_461_0 ))) ) * lerpResult372 ) , 0.0 , ( _SnowHeightBlendMask * HeightBlendMask96 ));
				float SnowMask260 = lerpResult309;
				float lerpResult587 = lerp( lerpResult140 , 1.0 , SnowMask260);
				float OcclusionOutput28 = lerpResult587;
				float lerpResult329 = lerp( saturate( ( saturate( (0.0 + (( saturate( ( worldNormal216.x * _MossAxis.x ) ) + saturate( ( worldNormal216.y * _MossAxis.y ) ) + saturate( ( worldNormal216.z * _MossAxis.z ) ) + _MossAxis.w ) - temp_output_348_0) * (( 2.0 * _MossStrength ) - 0.0) / (( temp_output_348_0 + _MossEdgeSoftness ) - temp_output_348_0)) ) * OcclusionOutput28 ) ) , 0.0 , ( _MossHeightBlendMask * HeightBlendMask96 ));
				float AlphaMask342 = tex2DNode10.a;
				float lerpResult467 = lerp( 1.0 , AlphaMask342 , _AlphaMask_Moss);
				float lerpResult474 = lerp( (1.0 + (AlphaMask342 - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) , 1.0 , ( _AlphaMask_Moss - -1.0 ));
				float temp_output_507_0 = ( saturate( lerpResult467 ) * saturate( lerpResult474 ) );
				float temp_output_462_0 = ( lerpResult329 * temp_output_507_0 );
				float MossMask227 = temp_output_462_0;
				float4 lerpResult272 = lerp( temp_output_159_0 , ( temp_output_270_0 + ( ( temp_output_270_0 * ( _MossColor * ( fresnelNode319 * _MossFresnelStrength ) ) ) * MaskmapOcclusion19 ) ) , MossMask227);
				#ifdef _MOSS_ON
				float4 staticSwitch280 = lerpResult272;
				#else
				float4 staticSwitch280 = temp_output_159_0;
				#endif
				float4 lerpResult99 = lerp( staticSwitch280 , ( staticSwitch280 * _HeightBlendColor ) , HeightBlendMask96);
				float4 lerpResult261 = lerp( lerpResult99 , ( _SnowColor * SnowMap_Albedo237 ) , SnowMask260);
				#ifdef _SNOW_ON
				float4 staticSwitch281 = lerpResult261;
				#else
				float4 staticSwitch281 = lerpResult99;
				#endif
				float4 AlbedoOutput39 = staticSwitch281;
				
				
				float3 Albedo = AlbedoOutput39.rgb;
				float3 Emission = 0;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				MetaInput metaInput = (MetaInput)0;
				metaInput.Albedo = Albedo;
				metaInput.Emission = Emission;
				
				return MetaFragment(metaInput);
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "Universal2D"
			Tags { "LightMode"="Universal2D" }

			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 100801

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_2D

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_COLOR
			#pragma shader_feature_local _SNOW_ON
			#pragma shader_feature_local _MOSS_ON
			#pragma shader_feature_local _DETAIL2_ON
			#pragma shader_feature_local _DETAIL_ON


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _SnowColor;
			float4 _NormalMap_ST;
			float4 _SnowMap_ST;
			float4 _MossAxis;
			float4 _MossMap_ST;
			float4 _MossColor;
			float4 _DetailMap0_ST;
			float4 _SnowAxis;
			float4 _DetailMap1_ST;
			float4 _BaseColorMap_ST;
			float4 _MaskMap_ST;
			float4 _HeightBlendColor;
			float3 _MossViewFresnel;
			float _SnowHeightBlendMask;
			float _HeightBlendOffset;
			float _MossHeightBlendMask;
			float _AlphaMask_Moss;
			float _Metallic;
			float _SnowBaseNormal;
			float _SnowOcclusionMask;
			float _Smoothness;
			float _Detail2SmoothnessScale;
			float _DetailSmoothnessScale;
			float _MossNormalStrength;
			float _SnowEdgeDetail;
			float _SnowMaskNormalInfluence;
			float _SnowMultiplier;
			float _SnowNormalStrength;
			float _MossSmoothness;
			float _OcclusionStrength;
			float _MossEdgeSoftness;
			float _MossStrength;
			float _MossNormalInfluence;
			float _DetailNormalMapScale;
			float _DetailNormalMapScale2;
			float _NormalStrength;
			float _MossFresnelStrength;
			float _DetailAlbedoStrength;
			float _Detail2AlbedoStrength;
			float _SnowExposureMask;
			float _HeightBlendSmoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _BaseColorMap;
			sampler2D _DetailMap1;
			sampler2D _DetailMap0;
			sampler2D _MaskMap;
			sampler2D _MossMap;
			sampler2D _NormalMap;
			sampler2D _SnowMap;
			float GlobalSnowStrength;
			float GlobalHeightBlend;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float3 normalizeWorldNormal = normalize( TransformObjectToWorldNormal(v.ase_normal) );
				o.ase_texcoord3.xyz = normalizeWorldNormal;
				float3 ase_worldTangent = TransformObjectToWorldDir(v.ase_tangent.xyz);
				o.ase_texcoord4.xyz = ase_worldTangent;
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord5.xyz = ase_worldNormal;
				float ase_vertexTangentSign = v.ase_tangent.w * ( unity_WorldTransformParams.w >= 0.0 ? 1.0 : -1.0 );
				float3 ase_worldBitangent = cross( ase_worldNormal, ase_worldTangent ) * ase_vertexTangentSign;
				o.ase_texcoord6.xyz = ase_worldBitangent;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				o.ase_texcoord3.w = 0;
				o.ase_texcoord4.w = 0;
				o.ase_texcoord5.w = 0;
				o.ase_texcoord6.w = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif

				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord = v.ase_texcoord;
				o.ase_tangent = v.ase_tangent;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_BaseColorMap = IN.ase_texcoord2.xy * _BaseColorMap_ST.xy + _BaseColorMap_ST.zw;
				float4 tex2DNode10 = tex2D( _BaseColorMap, uv_BaseColorMap );
				float4 temp_output_16_0 = ( float4( 1,1,1,0 ) * ( _BaseColor * tex2DNode10 ) );
				float4 appendResult200 = (float4(0.5 , 0.5 , 0.0 , 0.5));
				float4 Default188 = appendResult200;
				float2 uv_DetailMap1 = IN.ase_texcoord2.xy * _DetailMap1_ST.xy + _DetailMap1_ST.zw;
				#ifdef _DETAIL2_ON
				float4 staticSwitch191 = tex2D( _DetailMap1, uv_DetailMap1 );
				#else
				float4 staticSwitch191 = Default188;
				#endif
				float4 break202 = staticSwitch191;
				float lerpResult628 = lerp( 0.5 , break202.r , _Detail2AlbedoStrength);
				float DetailMap2_Albedo59 = lerpResult628;
				float2 uv_DetailMap0 = IN.ase_texcoord2.xy * _DetailMap0_ST.xy + _DetailMap0_ST.zw;
				#ifdef _DETAIL_ON
				float4 staticSwitch186 = tex2D( _DetailMap0, uv_DetailMap0 );
				#else
				float4 staticSwitch186 = Default188;
				#endif
				float4 break192 = staticSwitch186;
				float lerpResult626 = lerp( 0.5 , break192.r , _DetailAlbedoStrength);
				float DetailMap_Albedo53 = lerpResult626;
				float2 uv_MaskMap = IN.ase_texcoord2.xy * _MaskMap_ST.xy + _MaskMap_ST.zw;
				float4 tex2DNode14 = tex2D( _MaskMap, uv_MaskMap );
				float MaskmapDetailMask21 = tex2DNode14.b;
				float lerpResult168 = lerp( ( DetailMap2_Albedo59 * 2.0 ) , ( DetailMap_Albedo53 * 2.0 ) , MaskmapDetailMask21);
				float4 temp_output_159_0 = ( temp_output_16_0 * lerpResult168 );
				float2 uv_MossMap = IN.ase_texcoord2.xy * _MossMap_ST.xy + _MossMap_ST.zw;
				float4 tex2DNode205 = tex2D( _MossMap, uv_MossMap );
				float4 break246 = tex2DNode205;
				float MossMap_Albedo206 = break246.r;
				float4 temp_output_270_0 = ( MossMap_Albedo206 * _MossColor );
				float3 ase_worldViewDir = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				ase_worldViewDir = normalize(ase_worldViewDir);
				float3 normalizeWorldNormal = IN.ase_texcoord3.xyz;
				float fresnelNdotV319 = dot( normalizeWorldNormal, ase_worldViewDir );
				float fresnelNode319 = ( 0.0 + _MossViewFresnel.y * pow( max( 1.0 - fresnelNdotV319 , 0.0001 ), _MossViewFresnel.z ) );
				float MaskmapOcclusion19 = tex2DNode14.g;
				float3 unpack214 = UnpackNormalScale( float4( 0,0,0,0 ), 0.0 );
				unpack214.z = lerp( 1, unpack214.z, saturate(0.0) );
				float2 uv_NormalMap = IN.ase_texcoord2.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;
				float3 unpack42 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalStrength );
				unpack42.z = lerp( 1, unpack42.z, saturate(_NormalStrength) );
				float4 appendResult60 = (float4(break202.a , break202.g , 1.0 , 1.0));
				float3 unpack64 = UnpackNormalScale( appendResult60, _DetailNormalMapScale2 );
				unpack64.z = lerp( 1, unpack64.z, saturate(_DetailNormalMapScale2) );
				float3 DetailMap2_Normal66 = unpack64;
				float4 appendResult52 = (float4(break192.a , break192.g , 1.0 , 1.0));
				float3 unpack56 = UnpackNormalScale( appendResult52, _DetailNormalMapScale );
				unpack56.z = lerp( 1, unpack56.z, saturate(_DetailNormalMapScale) );
				float3 DetailMap_Normal51 = unpack56;
				float3 lerpResult81 = lerp( DetailMap2_Normal66 , DetailMap_Normal51 , MaskmapDetailMask21);
				float3 temp_output_55_0 = BlendNormal( unpack42 , lerpResult81 );
				float3 NormalBase287 = temp_output_55_0;
				float3 lerpResult215 = lerp( unpack214 , NormalBase287 , _MossNormalInfluence);
				float3 ase_worldTangent = IN.ase_texcoord4.xyz;
				float3 ase_worldNormal = IN.ase_texcoord5.xyz;
				float3 ase_worldBitangent = IN.ase_texcoord6.xyz;
				float3 tanToWorld0 = float3( ase_worldTangent.x, ase_worldBitangent.x, ase_worldNormal.x );
				float3 tanToWorld1 = float3( ase_worldTangent.y, ase_worldBitangent.y, ase_worldNormal.y );
				float3 tanToWorld2 = float3( ase_worldTangent.z, ase_worldBitangent.z, ase_worldNormal.z );
				float3 tanNormal216 = lerpResult215;
				float3 worldNormal216 = float3(dot(tanToWorld0,tanNormal216), dot(tanToWorld1,tanNormal216), dot(tanToWorld2,tanNormal216));
				float temp_output_348_0 = ( 1.0 - _MossStrength );
				float lerpResult140 = lerp( 1.0 , ( MaskmapOcclusion19 * IN.ase_color.g ) , _OcclusionStrength);
				float4 lerpResult293 = lerp( float4(0,0,1,0) , float4( NormalBase287 , 0.0 ) , _SnowMaskNormalInfluence);
				float2 uv_SnowMap = IN.ase_texcoord2.xy * _SnowMap_ST.xy + _SnowMap_ST.zw;
				float4 break236 = tex2D( _SnowMap, uv_SnowMap );
				float4 appendResult238 = (float4(break236.a , break236.g , 1.0 , 1.0));
				float3 unpack240 = UnpackNormalScale( appendResult238, _SnowNormalStrength );
				unpack240.z = lerp( 1, unpack240.z, saturate(_SnowNormalStrength) );
				#ifdef _SNOW_ON
				float4 staticSwitch235 = float4( unpack240 , 0.0 );
				#else
				float4 staticSwitch235 = float4(0,0,1,0);
				#endif
				float4 SnowMap_Normal241 = staticSwitch235;
				float saferPower249 = abs( ase_worldNormal.y );
				float4 lerpResult253 = lerp( lerpResult293 , SnowMap_Normal241 , saturate( (0.0 + (pow( saferPower249 , 3.0 ) - 0.0) * (1.0 - 0.0) / (1.0 - 0.0)) ));
				float3 tanNormal399 = lerpResult253.rgb;
				float3 worldNormal399 = float3(dot(tanToWorld0,tanNormal399), dot(tanToWorld1,tanNormal399), dot(tanToWorld2,tanNormal399));
				float temp_output_461_0 = ( GlobalSnowStrength * _SnowMultiplier );
				float ExposureMask375 = IN.ase_color.b;
				float lerpResult382 = lerp( ExposureMask375 , 1.0 , _SnowExposureMask);
				float SnowMap_Albedo237 = break236.r;
				float lerpResult372 = lerp( 1.0 , saturate( (1.0 + (( (1.0 + (( MaskmapOcclusion19 * lerpResult382 ) - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) / saturate( (0.0 + (SnowMap_Albedo237 - _SnowEdgeDetail) * (1.0 - 0.0) / (1.0 - _SnowEdgeDetail)) ) ) - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) ) , _SnowOcclusionMask);
				float temp_output_93_0 = ( GlobalHeightBlend + _HeightBlendOffset );
				float HeightBlendMask96 = saturate( (temp_output_93_0 + (-WorldPosition.y - 0.0) * (( temp_output_93_0 + 1.0 ) - temp_output_93_0) / (1.0 - 0.0)) );
				float lerpResult309 = lerp( ( saturate( (-4.1 + (saturate( ( saturate( ( worldNormal399.x * _SnowAxis.x ) ) + saturate( ( worldNormal399.y * _SnowAxis.y ) ) + saturate( ( worldNormal399.z * _SnowAxis.z ) ) ) ) - ( 1.0 - temp_output_461_0 )) * (( 1.9 * temp_output_461_0 ) - -4.1) / (1.0 - ( 1.0 - temp_output_461_0 ))) ) * lerpResult372 ) , 0.0 , ( _SnowHeightBlendMask * HeightBlendMask96 ));
				float SnowMask260 = lerpResult309;
				float lerpResult587 = lerp( lerpResult140 , 1.0 , SnowMask260);
				float OcclusionOutput28 = lerpResult587;
				float lerpResult329 = lerp( saturate( ( saturate( (0.0 + (( saturate( ( worldNormal216.x * _MossAxis.x ) ) + saturate( ( worldNormal216.y * _MossAxis.y ) ) + saturate( ( worldNormal216.z * _MossAxis.z ) ) + _MossAxis.w ) - temp_output_348_0) * (( 2.0 * _MossStrength ) - 0.0) / (( temp_output_348_0 + _MossEdgeSoftness ) - temp_output_348_0)) ) * OcclusionOutput28 ) ) , 0.0 , ( _MossHeightBlendMask * HeightBlendMask96 ));
				float AlphaMask342 = tex2DNode10.a;
				float lerpResult467 = lerp( 1.0 , AlphaMask342 , _AlphaMask_Moss);
				float lerpResult474 = lerp( (1.0 + (AlphaMask342 - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) , 1.0 , ( _AlphaMask_Moss - -1.0 ));
				float temp_output_507_0 = ( saturate( lerpResult467 ) * saturate( lerpResult474 ) );
				float temp_output_462_0 = ( lerpResult329 * temp_output_507_0 );
				float MossMask227 = temp_output_462_0;
				float4 lerpResult272 = lerp( temp_output_159_0 , ( temp_output_270_0 + ( ( temp_output_270_0 * ( _MossColor * ( fresnelNode319 * _MossFresnelStrength ) ) ) * MaskmapOcclusion19 ) ) , MossMask227);
				#ifdef _MOSS_ON
				float4 staticSwitch280 = lerpResult272;
				#else
				float4 staticSwitch280 = temp_output_159_0;
				#endif
				float4 lerpResult99 = lerp( staticSwitch280 , ( staticSwitch280 * _HeightBlendColor ) , HeightBlendMask96);
				float4 lerpResult261 = lerp( lerpResult99 , ( _SnowColor * SnowMap_Albedo237 ) , SnowMask260);
				#ifdef _SNOW_ON
				float4 staticSwitch281 = lerpResult261;
				#else
				float4 staticSwitch281 = lerpResult99;
				#endif
				float4 AlbedoOutput39 = staticSwitch281;
				
				
				float3 Albedo = AlbedoOutput39.rgb;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

				half4 color = half4( Albedo, Alpha );

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				return color;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthNormals"
			Tags { "LightMode"="DepthNormals" }

			ZWrite On
			Blend One Zero
            ZTest LEqual
            ZWrite On

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 100801

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHNORMALSONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float3 worldNormal : TEXCOORD2;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _SnowColor;
			float4 _NormalMap_ST;
			float4 _SnowMap_ST;
			float4 _MossAxis;
			float4 _MossMap_ST;
			float4 _MossColor;
			float4 _DetailMap0_ST;
			float4 _SnowAxis;
			float4 _DetailMap1_ST;
			float4 _BaseColorMap_ST;
			float4 _MaskMap_ST;
			float4 _HeightBlendColor;
			float3 _MossViewFresnel;
			float _SnowHeightBlendMask;
			float _HeightBlendOffset;
			float _MossHeightBlendMask;
			float _AlphaMask_Moss;
			float _Metallic;
			float _SnowBaseNormal;
			float _SnowOcclusionMask;
			float _Smoothness;
			float _Detail2SmoothnessScale;
			float _DetailSmoothnessScale;
			float _MossNormalStrength;
			float _SnowEdgeDetail;
			float _SnowMaskNormalInfluence;
			float _SnowMultiplier;
			float _SnowNormalStrength;
			float _MossSmoothness;
			float _OcclusionStrength;
			float _MossEdgeSoftness;
			float _MossStrength;
			float _MossNormalInfluence;
			float _DetailNormalMapScale;
			float _DetailNormalMapScale2;
			float _NormalStrength;
			float _MossFresnelStrength;
			float _DetailAlbedoStrength;
			float _Detail2AlbedoStrength;
			float _SnowExposureMask;
			float _HeightBlendSmoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			

			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 normalWS = TransformObjectToWorldNormal( v.ase_normal );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.worldNormal = normalWS;

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				
				#ifdef ASE_DEPTH_WRITE_ON
				outputDepth = DepthValue;
				#endif
				
				return float4(PackNormalOctRectEncode(TransformWorldToViewDir(IN.worldNormal, true)), 0.0, 0.0);
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "GBuffer"
			Tags { "LightMode"="UniversalGBuffer" }
			
			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 100801

			
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
			#pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			#pragma multi_compile _ _GBUFFER_NORMALS_OCT
			
			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_GBUFFER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/UnityGBuffer.hlsl"

			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#define ASE_NEEDS_FRAG_WORLD_VIEW_DIR
			#define ASE_NEEDS_FRAG_WORLD_NORMAL
			#define ASE_NEEDS_FRAG_WORLD_TANGENT
			#define ASE_NEEDS_FRAG_WORLD_BITANGENT
			#define ASE_NEEDS_FRAG_COLOR
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#pragma shader_feature_local _SNOW_ON
			#pragma shader_feature_local _MOSS_ON
			#pragma shader_feature_local _DETAIL2_ON
			#pragma shader_feature_local _DETAIL_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord : TEXCOORD0;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _SnowColor;
			float4 _NormalMap_ST;
			float4 _SnowMap_ST;
			float4 _MossAxis;
			float4 _MossMap_ST;
			float4 _MossColor;
			float4 _DetailMap0_ST;
			float4 _SnowAxis;
			float4 _DetailMap1_ST;
			float4 _BaseColorMap_ST;
			float4 _MaskMap_ST;
			float4 _HeightBlendColor;
			float3 _MossViewFresnel;
			float _SnowHeightBlendMask;
			float _HeightBlendOffset;
			float _MossHeightBlendMask;
			float _AlphaMask_Moss;
			float _Metallic;
			float _SnowBaseNormal;
			float _SnowOcclusionMask;
			float _Smoothness;
			float _Detail2SmoothnessScale;
			float _DetailSmoothnessScale;
			float _MossNormalStrength;
			float _SnowEdgeDetail;
			float _SnowMaskNormalInfluence;
			float _SnowMultiplier;
			float _SnowNormalStrength;
			float _MossSmoothness;
			float _OcclusionStrength;
			float _MossEdgeSoftness;
			float _MossStrength;
			float _MossNormalInfluence;
			float _DetailNormalMapScale;
			float _DetailNormalMapScale2;
			float _NormalStrength;
			float _MossFresnelStrength;
			float _DetailAlbedoStrength;
			float _Detail2AlbedoStrength;
			float _SnowExposureMask;
			float _HeightBlendSmoothness;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			sampler2D _BaseColorMap;
			sampler2D _DetailMap1;
			sampler2D _DetailMap0;
			sampler2D _MaskMap;
			sampler2D _MossMap;
			sampler2D _NormalMap;
			sampler2D _SnowMap;
			float GlobalSnowStrength;
			float GlobalHeightBlend;
			float SnowSmoothness;


			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord7.xy = v.texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					o.lightmapUVOrVertexSH.zw = v.texcoord;
					o.lightmapUVOrVertexSH.xy = v.texcoord * unity_LightmapST.xy + unity_LightmapST.zw;
				#endif

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
					half fogFactor = ComputeFogFactor( positionCS.z );
				#else
					half fogFactor = 0;
				#endif
				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			FragmentOutput frag ( VertexOutput IN 
								#ifdef ASE_DEPTH_WRITE_ON
								,out float outputDepth : ASE_SV_DEPTH
								#endif
								 )
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#endif
	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float2 uv_BaseColorMap = IN.ase_texcoord7.xy * _BaseColorMap_ST.xy + _BaseColorMap_ST.zw;
				float4 tex2DNode10 = tex2D( _BaseColorMap, uv_BaseColorMap );
				float4 temp_output_16_0 = ( float4( 1,1,1,0 ) * ( _BaseColor * tex2DNode10 ) );
				float4 appendResult200 = (float4(0.5 , 0.5 , 0.0 , 0.5));
				float4 Default188 = appendResult200;
				float2 uv_DetailMap1 = IN.ase_texcoord7.xy * _DetailMap1_ST.xy + _DetailMap1_ST.zw;
				#ifdef _DETAIL2_ON
				float4 staticSwitch191 = tex2D( _DetailMap1, uv_DetailMap1 );
				#else
				float4 staticSwitch191 = Default188;
				#endif
				float4 break202 = staticSwitch191;
				float lerpResult628 = lerp( 0.5 , break202.r , _Detail2AlbedoStrength);
				float DetailMap2_Albedo59 = lerpResult628;
				float2 uv_DetailMap0 = IN.ase_texcoord7.xy * _DetailMap0_ST.xy + _DetailMap0_ST.zw;
				#ifdef _DETAIL_ON
				float4 staticSwitch186 = tex2D( _DetailMap0, uv_DetailMap0 );
				#else
				float4 staticSwitch186 = Default188;
				#endif
				float4 break192 = staticSwitch186;
				float lerpResult626 = lerp( 0.5 , break192.r , _DetailAlbedoStrength);
				float DetailMap_Albedo53 = lerpResult626;
				float2 uv_MaskMap = IN.ase_texcoord7.xy * _MaskMap_ST.xy + _MaskMap_ST.zw;
				float4 tex2DNode14 = tex2D( _MaskMap, uv_MaskMap );
				float MaskmapDetailMask21 = tex2DNode14.b;
				float lerpResult168 = lerp( ( DetailMap2_Albedo59 * 2.0 ) , ( DetailMap_Albedo53 * 2.0 ) , MaskmapDetailMask21);
				float4 temp_output_159_0 = ( temp_output_16_0 * lerpResult168 );
				float2 uv_MossMap = IN.ase_texcoord7.xy * _MossMap_ST.xy + _MossMap_ST.zw;
				float4 tex2DNode205 = tex2D( _MossMap, uv_MossMap );
				float4 break246 = tex2DNode205;
				float MossMap_Albedo206 = break246.r;
				float4 temp_output_270_0 = ( MossMap_Albedo206 * _MossColor );
				float fresnelNdotV319 = dot( normalize( WorldNormal ), WorldViewDirection );
				float fresnelNode319 = ( 0.0 + _MossViewFresnel.y * pow( max( 1.0 - fresnelNdotV319 , 0.0001 ), _MossViewFresnel.z ) );
				float MaskmapOcclusion19 = tex2DNode14.g;
				float3 unpack214 = UnpackNormalScale( float4( 0,0,0,0 ), 0.0 );
				unpack214.z = lerp( 1, unpack214.z, saturate(0.0) );
				float2 uv_NormalMap = IN.ase_texcoord7.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;
				float3 unpack42 = UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), _NormalStrength );
				unpack42.z = lerp( 1, unpack42.z, saturate(_NormalStrength) );
				float4 appendResult60 = (float4(break202.a , break202.g , 1.0 , 1.0));
				float3 unpack64 = UnpackNormalScale( appendResult60, _DetailNormalMapScale2 );
				unpack64.z = lerp( 1, unpack64.z, saturate(_DetailNormalMapScale2) );
				float3 DetailMap2_Normal66 = unpack64;
				float4 appendResult52 = (float4(break192.a , break192.g , 1.0 , 1.0));
				float3 unpack56 = UnpackNormalScale( appendResult52, _DetailNormalMapScale );
				unpack56.z = lerp( 1, unpack56.z, saturate(_DetailNormalMapScale) );
				float3 DetailMap_Normal51 = unpack56;
				float3 lerpResult81 = lerp( DetailMap2_Normal66 , DetailMap_Normal51 , MaskmapDetailMask21);
				float3 temp_output_55_0 = BlendNormal( unpack42 , lerpResult81 );
				float3 NormalBase287 = temp_output_55_0;
				float3 lerpResult215 = lerp( unpack214 , NormalBase287 , _MossNormalInfluence);
				float3 tanToWorld0 = float3( WorldTangent.x, WorldBiTangent.x, WorldNormal.x );
				float3 tanToWorld1 = float3( WorldTangent.y, WorldBiTangent.y, WorldNormal.y );
				float3 tanToWorld2 = float3( WorldTangent.z, WorldBiTangent.z, WorldNormal.z );
				float3 tanNormal216 = lerpResult215;
				float3 worldNormal216 = float3(dot(tanToWorld0,tanNormal216), dot(tanToWorld1,tanNormal216), dot(tanToWorld2,tanNormal216));
				float temp_output_348_0 = ( 1.0 - _MossStrength );
				float lerpResult140 = lerp( 1.0 , ( MaskmapOcclusion19 * IN.ase_color.g ) , _OcclusionStrength);
				float4 lerpResult293 = lerp( float4(0,0,1,0) , float4( NormalBase287 , 0.0 ) , _SnowMaskNormalInfluence);
				float2 uv_SnowMap = IN.ase_texcoord7.xy * _SnowMap_ST.xy + _SnowMap_ST.zw;
				float4 break236 = tex2D( _SnowMap, uv_SnowMap );
				float4 appendResult238 = (float4(break236.a , break236.g , 1.0 , 1.0));
				float3 unpack240 = UnpackNormalScale( appendResult238, _SnowNormalStrength );
				unpack240.z = lerp( 1, unpack240.z, saturate(_SnowNormalStrength) );
				#ifdef _SNOW_ON
				float4 staticSwitch235 = float4( unpack240 , 0.0 );
				#else
				float4 staticSwitch235 = float4(0,0,1,0);
				#endif
				float4 SnowMap_Normal241 = staticSwitch235;
				float saferPower249 = abs( WorldNormal.y );
				float4 lerpResult253 = lerp( lerpResult293 , SnowMap_Normal241 , saturate( (0.0 + (pow( saferPower249 , 3.0 ) - 0.0) * (1.0 - 0.0) / (1.0 - 0.0)) ));
				float3 tanNormal399 = lerpResult253.rgb;
				float3 worldNormal399 = float3(dot(tanToWorld0,tanNormal399), dot(tanToWorld1,tanNormal399), dot(tanToWorld2,tanNormal399));
				float temp_output_461_0 = ( GlobalSnowStrength * _SnowMultiplier );
				float ExposureMask375 = IN.ase_color.b;
				float lerpResult382 = lerp( ExposureMask375 , 1.0 , _SnowExposureMask);
				float SnowMap_Albedo237 = break236.r;
				float lerpResult372 = lerp( 1.0 , saturate( (1.0 + (( (1.0 + (( MaskmapOcclusion19 * lerpResult382 ) - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) / saturate( (0.0 + (SnowMap_Albedo237 - _SnowEdgeDetail) * (1.0 - 0.0) / (1.0 - _SnowEdgeDetail)) ) ) - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) ) , _SnowOcclusionMask);
				float temp_output_93_0 = ( GlobalHeightBlend + _HeightBlendOffset );
				float HeightBlendMask96 = saturate( (temp_output_93_0 + (-WorldPosition.y - 0.0) * (( temp_output_93_0 + 1.0 ) - temp_output_93_0) / (1.0 - 0.0)) );
				float lerpResult309 = lerp( ( saturate( (-4.1 + (saturate( ( saturate( ( worldNormal399.x * _SnowAxis.x ) ) + saturate( ( worldNormal399.y * _SnowAxis.y ) ) + saturate( ( worldNormal399.z * _SnowAxis.z ) ) ) ) - ( 1.0 - temp_output_461_0 )) * (( 1.9 * temp_output_461_0 ) - -4.1) / (1.0 - ( 1.0 - temp_output_461_0 ))) ) * lerpResult372 ) , 0.0 , ( _SnowHeightBlendMask * HeightBlendMask96 ));
				float SnowMask260 = lerpResult309;
				float lerpResult587 = lerp( lerpResult140 , 1.0 , SnowMask260);
				float OcclusionOutput28 = lerpResult587;
				float lerpResult329 = lerp( saturate( ( saturate( (0.0 + (( saturate( ( worldNormal216.x * _MossAxis.x ) ) + saturate( ( worldNormal216.y * _MossAxis.y ) ) + saturate( ( worldNormal216.z * _MossAxis.z ) ) + _MossAxis.w ) - temp_output_348_0) * (( 2.0 * _MossStrength ) - 0.0) / (( temp_output_348_0 + _MossEdgeSoftness ) - temp_output_348_0)) ) * OcclusionOutput28 ) ) , 0.0 , ( _MossHeightBlendMask * HeightBlendMask96 ));
				float AlphaMask342 = tex2DNode10.a;
				float lerpResult467 = lerp( 1.0 , AlphaMask342 , _AlphaMask_Moss);
				float lerpResult474 = lerp( (1.0 + (AlphaMask342 - 0.0) * (0.0 - 1.0) / (1.0 - 0.0)) , 1.0 , ( _AlphaMask_Moss - -1.0 ));
				float temp_output_507_0 = ( saturate( lerpResult467 ) * saturate( lerpResult474 ) );
				float temp_output_462_0 = ( lerpResult329 * temp_output_507_0 );
				float MossMask227 = temp_output_462_0;
				float4 lerpResult272 = lerp( temp_output_159_0 , ( temp_output_270_0 + ( ( temp_output_270_0 * ( _MossColor * ( fresnelNode319 * _MossFresnelStrength ) ) ) * MaskmapOcclusion19 ) ) , MossMask227);
				#ifdef _MOSS_ON
				float4 staticSwitch280 = lerpResult272;
				#else
				float4 staticSwitch280 = temp_output_159_0;
				#endif
				float4 lerpResult99 = lerp( staticSwitch280 , ( staticSwitch280 * _HeightBlendColor ) , HeightBlendMask96);
				float4 lerpResult261 = lerp( lerpResult99 , ( _SnowColor * SnowMap_Albedo237 ) , SnowMask260);
				#ifdef _SNOW_ON
				float4 staticSwitch281 = lerpResult261;
				#else
				float4 staticSwitch281 = lerpResult99;
				#endif
				float4 AlbedoOutput39 = staticSwitch281;
				
				float4 appendResult208 = (float4(break246.a , break246.g , 1.0 , 1.0));
				float3 unpack209 = UnpackNormalScale( appendResult208, _MossNormalStrength );
				unpack209.z = lerp( 1, unpack209.z, saturate(_MossNormalStrength) );
				float3 MossMap_Normal210 = unpack209;
				float3 lerpResult330 = lerp( temp_output_55_0 , MossMap_Normal210 , MossMask227);
				#ifdef _MOSS_ON
				float3 staticSwitch333 = lerpResult330;
				#else
				float3 staticSwitch333 = temp_output_55_0;
				#endif
				float4 lerpResult290 = lerp( SnowMap_Normal241 , float4( BlendNormal( staticSwitch333 , SnowMap_Normal241.rgb ) , 0.0 ) , _SnowBaseNormal);
				float4 lerpResult285 = lerp( float4( staticSwitch333 , 0.0 ) , lerpResult290 , SnowMask260);
				#ifdef _SNOW_ON
				float4 staticSwitch334 = lerpResult285;
				#else
				float4 staticSwitch334 = float4( staticSwitch333 , 0.0 );
				#endif
				float4 NormalOutput67 = staticSwitch334;
				
				float MaskmapMetallic22 = tex2DNode14.r;
				float MetallicOutput35 = ( MaskmapMetallic22 * _Metallic );
				
				float MaskmapSmoothness20 = tex2DNode14.a;
				float DetailMap2_Smoothness65 = ( break202.b * _Detail2SmoothnessScale );
				float DetailMap_Smoothness54 = ( break192.b * _DetailSmoothnessScale );
				float lerpResult630 = lerp( ( DetailMap2_Smoothness65 * 2.0 ) , ( DetailMap_Smoothness54 * 2.0 ) , MaskmapDetailMask21);
				float temp_output_158_0 = ( ( MaskmapSmoothness20 * _Smoothness ) + lerpResult630 );
				float MossMap_Smoothness207 = break246.b;
				float lerpResult339 = lerp( temp_output_158_0 , ( MossMap_Smoothness207 * _MossSmoothness ) , MossMask227);
				#ifdef _MOSS_ON
				float staticSwitch341 = lerpResult339;
				#else
				float staticSwitch341 = temp_output_158_0;
				#endif
				float SnowMap_Smoothness243 = break236.b;
				float lerpResult300 = lerp( staticSwitch341 , ( SnowMap_Smoothness243 * SnowSmoothness ) , SnowMask260);
				#ifdef _SNOW_ON
				float staticSwitch340 = lerpResult300;
				#else
				float staticSwitch340 = staticSwitch341;
				#endif
				float lerpResult136 = lerp( staticSwitch340 , ( staticSwitch340 + _HeightBlendSmoothness ) , HeightBlendMask96);
				float SmoothnessOutput31 = lerpResult136;
				
				float3 Albedo = AlbedoOutput39.rgb;
				float3 Normal = NormalOutput67.rgb;
				float3 Emission = 0;
				float3 Specular = 0.5;
				float Metallic = MetallicOutput35;
				float Smoothness = SmoothnessOutput31;
				float Occlusion = OcclusionOutput28;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				float AlphaClipThresholdShadow = 0.5;
				float3 BakedGI = 0;
				float3 RefractionColor = 1;
				float RefractionIndex = 1;
				float3 Transmission = 1;
				float3 Translucency = 1;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
					inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				#else
					inputData.normalWS = WorldNormal;
				#endif

				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
				#endif

				BRDFData brdfData;
				InitializeBRDFData( Albedo, Metallic, Specular, Smoothness, Alpha, brdfData);
				half4 color;
				color.rgb = GlobalIllumination( brdfData, inputData.bakedGI, Occlusion, inputData.normalWS, inputData.viewDirectionWS);
				color.a = Alpha;

				#ifdef _TRANSMISSION_ASE
				{
					float shadow = _TransmissionShadow;
				
					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
					half3 mainTransmission = max(0 , -dot(inputData.normalWS, mainLight.direction)) * mainAtten * Transmission;
					color.rgb += Albedo * mainTransmission;
				
					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );
				
							half3 transmission = max(0 , -dot(inputData.normalWS, light.direction)) * atten * Transmission;
							color.rgb += Albedo * transmission;
						}
					#endif
				}
				#endif
				
				#ifdef _TRANSLUCENCY_ASE
				{
					float shadow = _TransShadow;
					float normal = _TransNormal;
					float scattering = _TransScattering;
					float direct = _TransDirect;
					float ambient = _TransAmbient;
					float strength = _TransStrength;
				
					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
				
					half3 mainLightDir = mainLight.direction + inputData.normalWS * normal;
					half mainVdotL = pow( saturate( dot( inputData.viewDirectionWS, -mainLightDir ) ), scattering );
					half3 mainTranslucency = mainAtten * ( mainVdotL * direct + inputData.bakedGI * ambient ) * Translucency;
					color.rgb += Albedo * mainTranslucency * strength;
				
					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );
				
							half3 lightDir = light.direction + inputData.normalWS * normal;
							half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );
							half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;
							color.rgb += Albedo * translucency * strength;
						}
					#endif
				}
				#endif
				
				#ifdef _REFRACTION_ASE
					float4 projScreenPos = ScreenPos / ScreenPos.w;
					float3 refractionOffset = ( RefractionIndex - 1.0 ) * mul( UNITY_MATRIX_V, float4( WorldNormal, 0 ) ).xyz * ( 1.0 - dot( WorldNormal, WorldViewDirection ) );
					projScreenPos.xy += refractionOffset.xy;
					float3 refraction = SHADERGRAPH_SAMPLE_SCENE_COLOR( projScreenPos.xy ) * RefractionColor;
					color.rgb = lerp( refraction, color.rgb, color.a );
					color.a = 1;
				#endif
				
				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif
				
				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif
				
				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif
				
				return BRDFDataToGbuffer(brdfData, inputData, Smoothness, Emission + color.rgb);
			}

			ENDHLSL
		}
		
	}
	
	CustomEditor "UnityEditor.ShaderGraph.PBRMasterGUI"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=19102
Node;AmplifyShaderEditor.DynamicAppendNode;200;-4414.059,-1094.061;Inherit;False;COLOR;4;0;FLOAT;0.5;False;1;FLOAT;0.5;False;2;FLOAT;0;False;3;FLOAT;0.5;False;1;COLOR;0
Node;AmplifyShaderEditor.CommentaryNode;58;-4762.239,888.7983;Inherit;False;3112.127;712.9756;;15;65;59;66;64;63;60;202;191;203;62;61;623;624;627;628;Detail Map 2;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;50;-4759.386,129.086;Inherit;False;3109.276;676.8381;;15;186;201;49;54;53;51;56;52;57;192;48;622;621;625;626;Detail Map;1,1,1,1;0;0
Node;AmplifyShaderEditor.TexturePropertyNode;61;-4712.239,938.7983;Inherit;True;Property;_DetailMap1;Detail Map 2;22;0;Create;False;1;Detail Map;0;0;False;0;False;None;d76baa906337a3f44989e463a0603ed6;False;gray;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RegisterLocalVarNode;188;-4253.229,-1086.51;Inherit;False;Default;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SamplerNode;62;-4446.609,936.8063;Inherit;True;Property;_TextureSample4;Texture Sample 4;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;203;-4318.047,1186.195;Inherit;False;188;Default;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;191;-4067.307,949.9678;Inherit;False;Property;_Detail2;Detail 2;21;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.BreakToComponentsNode;202;-3760.782,959.6292;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.CommentaryNode;23;-1413.543,140.2957;Inherit;False;1195.498;417.6917;;5;13;22;19;21;20;MaskMap;1,1,1,1;0;0
Node;AmplifyShaderEditor.DynamicAppendNode;60;-3449.308,1028.609;Inherit;False;COLOR;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;1;COLOR;0
Node;AmplifyShaderEditor.UnpackScaleNormalNode;64;-3182.615,1156.951;Inherit;False;Tangent;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SamplerNode;14;-1010.518,238.1553;Inherit;True;Property;_TextureSample1;Texture Sample 1;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;66;-2881.128,1144.261;Inherit;False;DetailMap2_Normal;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;234;-13302.67,3210.393;Inherit;True;Property;_TextureSample6;Texture Sample 6;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;43;-7985.129,-3307.718;Inherit;False;4119.874;1430.187;;2;41;44;Normals;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;80;-7600.489,-2883.448;Inherit;False;66;DetailMap2_Normal;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;72;-7600.787,-2777.126;Inherit;False;21;MaskmapDetailMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;69;-7595.715,-2984.163;Inherit;False;51;DetailMap_Normal;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.BreakToComponentsNode;236;-12928.65,3234.742;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.DynamicAppendNode;238;-12610.96,3303.993;Inherit;False;COLOR;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;1;COLOR;0
Node;AmplifyShaderEditor.SamplerNode;42;-7605.82,-3210.916;Inherit;True;Property;_TextureSample2;Texture Sample 2;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.LerpOp;81;-7252.381,-2982.664;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;239;-12699.99,3456.338;Inherit;False;Property;_SnowNormalStrength;Snow Normal Strength;43;0;Create;True;0;0;0;False;0;False;1;1;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.WorldNormalVector;248;-9851.222,3227.805;Inherit;True;False;1;0;FLOAT3;0,0,1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.UnpackScaleNormalNode;240;-12331.08,3451.464;Inherit;False;Tangent;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.BlendNormalsNode;55;-7027.817,-3072.435;Inherit;False;0;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;287;-6733.542,-3164.825;Inherit;False;NormalBase;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;241;-11391.15,3417.529;Inherit;False;SnowMap_Normal;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.TFHCRemapNode;251;-9246.994,3348.901;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;298;-9513.106,2917.563;Inherit;False;Constant;_NormalBlank;Normal Blank;39;0;Create;True;0;0;0;False;0;False;0,0,1,0;0,0,0,0;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.VertexColorNode;374;-8355.724,4697.432;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;256;-9548.611,3100.422;Inherit;False;287;NormalBase;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;292;-9585.438,3194.182;Inherit;False;Property;_SnowMaskNormalInfluence;Snow Mask Normal Influence;46;0;Create;True;0;0;0;False;0;False;0.75;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;252;-9036.692,3389.802;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;365;-7460.151,2919.548;Inherit;True;19;MaskmapOcclusion;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;293;-9113.987,3102.878;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;255;-9246.313,3258.838;Inherit;False;241;SnowMap_Normal;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;237;-12658.3,3217.555;Inherit;False;SnowMap_Albedo;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;253;-8853.556,3236.837;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;378;-7039.851,3589.395;Inherit;False;375;ExposureMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;456;-7136.562,3085.866;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;379;-7061.759,3736.338;Inherit;False;Property;_SnowExposureMask;Snow Exposure Mask;49;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector4Node;577;-8517.587,3118.392;Inherit;False;Property;_SnowAxis;Snow Axis;58;0;Create;True;0;0;0;False;0;False;0,1,0,0;0,1,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.WorldNormalVector;399;-8545.807,2895.962;Inherit;True;False;1;0;FLOAT3;0,0,1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.LerpOp;382;-6738.476,3527.395;Inherit;True;3;0;FLOAT;1;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ToggleSwitchNode;459;-6818.267,3049.682;Inherit;False;Property;_SnowOcclusionInvert;Snow Occlusion Invert;54;0;Create;True;0;0;0;False;0;False;0;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;384;-6569.263,3845.434;Inherit;False;Property;_SnowEdgeDetail;Snow Edge Detail;48;0;Create;True;0;0;0;False;0;False;0;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;390;-6429.189,3718.6;Inherit;False;237;SnowMap_Albedo;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;401;-8198.908,3007.109;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;402;-8199.404,3112.955;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;381;-6295.197,3468.635;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;400;-8197.608,2899.209;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldPosInputsNode;88;-3416.944,1823.492;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.TFHCRemapNode;391;-6141.888,3715.803;Inherit;True;5;0;FLOAT;1;False;1;FLOAT;0.71;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;93;-3270.746,2078.584;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;259;-9878.146,2839.256;Inherit;False;6956.507;1306.16;;5;276;258;460;249;250;Snow Mask;1,1,1,1;0;0
Node;AmplifyShaderEditor.NegateNode;89;-3153.125,1877.609;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;394;-5909.424,3461.391;Inherit;True;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;580;-8027.524,3086.572;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;392;-5851.076,3716.437;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;579;-8030.565,3002.937;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;578;-8019.918,2917.78;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;274;-8797.253,3550.288;Inherit;False;Constant;_Float3;Float 3;36;0;Create;True;0;0;0;False;0;False;1.9;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;403;-7706.644,3017.019;Inherit;True;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;283;-8795.079,3389.171;Inherit;False;Constant;_Float4;Float 4;37;0;Create;True;0;0;0;False;0;False;1;-4.1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;395;-5519.827,3456.135;Inherit;True;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;461;-8849.408,3713.639;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;396;-5263.717,3450.586;Inherit;True;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;583;-7419.452,3198.557;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;273;-8797.253,3471.289;Inherit;False;Constant;_Float2;Float 2;37;0;Create;True;0;0;0;False;0;False;-4.1;-4.1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;282;-8512.685,3372.491;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;279;-8566.471,3582.501;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;91;-2723.331,1880.057;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;393;-4960.374,3406.703;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;96;-2549.887,1880.644;Inherit;False;HeightBlendMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;363;-5030.285,3719.267;Inherit;False;Property;_SnowOcclusionMask;Snow Occlusion Mask;47;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;257;-8280.958,3361.699;Inherit;True;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;-4.1;False;4;FLOAT;1.9;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;372;-4695.428,3339.337;Inherit;True;3;0;FLOAT;1;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;310;-4039.962,3503.185;Inherit;False;Property;_SnowHeightBlendMask;Snow Height Blend Mask;50;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;304;-3995.273,3625.377;Inherit;False;96;HeightBlendMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;311;-3692.236,3516.39;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;373;-4298.929,3175.669;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.VertexColorNode;25;-1544.182,1024.173;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;38;-8496.701,-6211.093;Inherit;False;8825.773;2006.393;;6;9;169;149;162;11;16;Albedo;1,1,1,1;0;0
Node;AmplifyShaderEditor.LerpOp;309;-3477.524,3357.581;Inherit;True;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldNormalVector;216;-13518.07,-412.2235;Inherit;True;False;1;0;FLOAT3;0,0,1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.CommentaryNode;26;-1577.787,852.8704;Inherit;False;1626.953;738.9094;;4;46;28;140;587;Occlusion;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;27;-1525.098,932.7051;Inherit;False;19;MaskmapOcclusion;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;219;-13163.56,-386.4972;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;218;-13161.19,-284.7435;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;260;-3145.52,3343.655;Inherit;False;SnowMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;18;-1236.444,1043.96;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;542;-13015.87,-485.5659;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;10;-8092.101,-5888.488;Inherit;True;Property;_TextureSample0;Texture Sample 0;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SaturateNode;543;-13015.53,-385.1807;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;584;-1340.892,1324.279;Inherit;False;260;SnowMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;544;-13018.79,-282.0446;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;222;-12673.17,-320.8186;Inherit;True;4;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;342;-7717.555,-5731.852;Inherit;False;AlphaMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;205;-7288.911,-1272.353;Inherit;True;Property;_TextureSample3;Texture Sample 3;0;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;267;-5484.813,-6052.58;Inherit;False;2308.98;1327.205;;3;316;280;269;Moss Albedo;1,1,1,1;0;0
Node;AmplifyShaderEditor.BreakToComponentsNode;246;-6661.799,-1209.114;Inherit;True;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.Vector3Node;315;-5178.277,-5130.113;Inherit;False;Property;_MossViewFresnel;Moss View Fresnel;37;0;Create;True;0;0;0;False;0;False;0,2.86,2;0,4,1.3;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;327;-11224.65,6.496467;Inherit;False;96;HeightBlendMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;316;-5208.57,-4866.39;Inherit;False;Property;_MossFresnelStrength;Moss Fresnel Strength;36;0;Create;True;0;0;0;False;0;False;0;0.0002;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;225;-11363.25,-286.1157;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FresnelNode;319;-4933.558,-5148.57;Inherit;False;Standard;WorldNormal;ViewDir;True;True;5;0;FLOAT3;0,0,1;False;4;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;2;False;3;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;326;-11256.34,-113.6956;Inherit;False;Property;_MossHeightBlendMask;Moss Height Blend Mask;34;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;206;-6380.083,-1264.111;Inherit;False;MossMap_Albedo;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;492;-9907.543,464.6941;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;268;-5434.527,-5830.562;Inherit;False;206;MossMap_Albedo;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;163;-6856.482,-5669.087;Inherit;False;59;DetailMap2_Albedo;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;141;-6861.788,-5770.892;Inherit;False;53;DetailMap_Albedo;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;328;-10894.12,-130.176;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.VertexColorNode;15;-6859.053,-6148.145;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SaturateNode;226;-11123.25,-273.7237;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;494;-9933.631,189.8615;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;320;-4658.784,-5059.547;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;322;-5005.399,-5691.309;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;329;-10685.87,-261.3899;Inherit;True;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;507;-9635.957,155.0323;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;17;-6660.826,-6123.113;Inherit;False;COLOR;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;170;-6651.208,-5545.646;Inherit;False;21;MaskmapDetailMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;12;-6781.642,-5899.273;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;270;-5002.953,-5814.82;Inherit;False;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.DynamicAppendNode;208;-6346.678,-1099.736;Inherit;False;COLOR;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;1;COLOR;0
Node;AmplifyShaderEditor.UnpackScaleNormalNode;209;-6171.059,-1089.998;Inherit;False;Tangent;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.LerpOp;168;-6344.067,-5780.187;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;312;-4773.005,-5608.049;Inherit;False;19;MaskmapOcclusion;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;207;-6371.549,-1182.109;Inherit;False;MossMap_Smoothness;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;30;1457.266,1120.642;Inherit;False;20;MaskmapSmoothness;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;313;-4696.792,-5729.708;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;323;-4495.485,-5742.133;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;335;3402.532,1335.883;Inherit;False;207;MossMap_Smoothness;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;47;1777.072,1146.747;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;159;-6121.199,-5910.294;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;2;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;332;-6630.45,-2813.022;Inherit;False;227;MossMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;337;3701.12,1469.241;Inherit;False;227;MossMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;331;-6655.796,-2959.31;Inherit;False;210;MossMap_Normal;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;243;-12699.76,3558.489;Inherit;False;SnowMap_Smoothness;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;338;3698.549,1345.888;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;271;-4105.622,-5722.626;Inherit;False;227;MossMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;158;2363.327,1166.062;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;324;-4251.186,-5829.032;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;301;4259.354,1308.673;Inherit;False;243;SnowMap_Smoothness;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;339;3944.77,1251.837;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;330;-6292.546,-3023.205;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;272;-3904.751,-5892.101;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;299;4536.846,1441.884;Inherit;False;260;SnowMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;333;-6041.172,-3126.049;Inherit;False;Property;_Keyword1;Keyword 1;26;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;245;True;True;All;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;284;-5900.768,-2916.244;Inherit;False;241;SnowMap_Normal;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;302;4534.276,1308.13;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;341;4255.367,1134.82;Inherit;False;Property;_Keyword2;Keyword 2;26;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;245;True;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;100;-2219.001,-5866.297;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;300;4796.471,1249.684;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;289;-5644.768,-2820.244;Inherit;False;Property;_SnowBaseNormal;Snow Base Normal;45;0;Create;True;0;0;0;False;0;False;0.75;0.6;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.BlendNormalsNode;288;-5564.768,-2996.244;Inherit;False;0;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;101;-2301.635,-5721.909;Inherit;False;96;HeightBlendMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;262;-1303.027,-5569.126;Inherit;False;237;SnowMap_Albedo;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;263;-1015.935,-5586.96;Inherit;False;260;SnowMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;265;-1026.925,-5718.166;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;99;-1938.749,-5931.342;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;340;5093.413,1140.549;Inherit;False;Property;_Keyword2;Keyword 2;40;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;235;True;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;286;-5292.768,-2804.244;Inherit;False;260;SnowMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;290;-5276.768,-2932.244;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;261;-834.1,-5811.684;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;83;115.4133,1359.516;Inherit;False;Property;_Metallic;Metallic;9;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;34;146.1158,1210.15;Inherit;False;22;MaskmapMetallic;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;285;-5004.768,-3060.244;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;135;5707.685,1288.914;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;134;5598.217,1451.673;Inherit;False;96;HeightBlendMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;334;-4765.054,-3158.502;Inherit;False;Property;_Keyword1;Keyword 1;40;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;235;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;136;5871.887,1219.774;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;84;471.8981,1322.748;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;35;724.055,1204.358;Inherit;False;MetallicOutput;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;31;6282.317,1169.612;Inherit;False;SmoothnessOutput;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;39;-112.4522,-6002.98;Inherit;False;AlbedoOutput;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;67;-4364.768,-3076.244;Inherit;False;NormalOutput;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;65;-3531.889,1283.377;Inherit;False;DetailMap2_Smoothness;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;314;-5427.91,-4955.974;Inherit;False;210;MossMap_Normal;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;445;4929.371,3809.007;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;37;751.4858,220.9064;Inherit;False;35;MetallicOutput;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;109;389.1694,128.2316;Inherit;False;108;EmissionOutput;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.IndirectDiffuseLighting;317;-5174.309,-4962.139;Inherit;False;Tangent;1;0;FLOAT3;0,0,1;False;1;FLOAT3;0
Node;AmplifyShaderEditor.OneMinusNode;448;4191.854,3852.496;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;108;5681.518,3746.491;Inherit;False;EmissionOutput;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;440;702.3961,89.48871;Inherit;False;Property;_Emission;Emission;12;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;32;730.0491,302.6022;Inherit;False;31;SmoothnessOutput;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;450;4643.48,3639.488;Inherit;False;Property;_Keyword3;Keyword 3;26;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;245;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;441;449.4554,8.83786;Inherit;False;Constant;_EmissionOff;Emission Off;53;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;449;5204.959,3693.31;Inherit;False;Property;_Keyword3;Keyword 3;39;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;235;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;453;4428.117,3756.606;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.OneMinusNode;447;4213.992,4044.8;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;40;759.0483,-89.74905;Inherit;False;39;AlbedoOutput;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.FunctionNode;439;-6373.534,-5295.308;Inherit;False;Detail Albedo;0;;1;29e5a290b15a7884983e27c8f1afaa8c;0;3;12;FLOAT3;0,0,0;False;11;FLOAT3;0,0,0;False;9;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;68;754.0823,-13.71857;Inherit;False;67;NormalOutput;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.SamplerNode;103;1807.632,3584.752;Inherit;True;Property;_TextureSample5;Texture Sample 5;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;444;3932.506,3862.2;Inherit;False;227;MossMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;442;1775.408,3814.119;Inherit;False;Property;_EmissionColor2;Emission Color;13;1;[HDR];Create;False;0;0;0;False;0;False;0,0,0,0;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;247;-7194.313,-1018.857;Inherit;False;188;Default;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;29;747.9322,389.7374;Inherit;False;28;OcclusionOutput;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;106;2324.576,3541.282;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.TexturePropertyNode;102;1483.786,3593.31;Inherit;True;Property;_EmissionMap;Emission Map;14;0;Create;True;0;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.GetLocalVarNode;443;3922.717,4074.525;Inherit;False;260;SnowMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;318;-4896.457,-4966.363;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;36;96.1158,1154.358;Inherit;False;1122.903;405.7836;;0;Metallic;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;97;-3702.989,1705.25;Inherit;False;1651.409;794.9987;;4;92;95;90;94;Height Blend Mask;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;321;-5465.62,-5198.569;Inherit;False;970.168;432.437;;0;Moss Fresnel;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;230;-14133.96,-540.4247;Inherit;False;9285.059;3097.323;;17;214;212;213;546;220;227;348;224;362;345;350;344;465;477;474;467;473;Moss Mask;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;231;-13829.2,3158.959;Inherit;False;2738.743;612.098;;3;354;232;235;Snow Map;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;187;-4477.546,-1155.069;Inherit;False;446.2468;239.3462;;0;Default;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;325;-2616.002,-5983.109;Inherit;False;927.1338;427.6042;;1;98;Height Blend Albedo;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;104;1458.043,3470.034;Inherit;False;4744.294;1246.959;;11;127;114;129;119;113;110;120;130;131;124;117;Emission;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;266;-1342.272,-6026.246;Inherit;False;934.4816;554.6343;;2;281;264;Snow Albedo;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;33;1407.266,1068.438;Inherit;False;5142.818;1491.241;;10;151;150;630;631;632;633;45;336;137;303;Smoothness;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;211;-7694.544,-1326.053;Inherit;False;2666.613;651.9974;;4;210;245;204;244;Moss Map;1,1,1,1;0;0
Node;AmplifyShaderEditor.HeightMapBlendNode;564;-7499.187,54.65076;Inherit;True;True;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;615;-8887.975,156.5017;Inherit;True;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;566;-7987.303,603.0928;Inherit;False;Property;_asdf;asdf;38;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;483;-7999.151,330.984;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;549;-8820.834,401.255;Inherit;False;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;486;-9215.199,559.509;Inherit;False;Property;_MossAlbedoClip;Moss Albedo Clip;35;0;Create;True;0;0;0;False;0;False;0.5;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;484;-8287.703,335.1271;Inherit;True;5;0;FLOAT;0;False;1;FLOAT;0.71;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;114;2326.853,4211.83;Inherit;False;Property;_EmissionDetail;Emission Detail;15;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;129;2973.862,3700.604;Inherit;True;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.Vector4Node;119;1995.719,4064.286;Inherit;False;Property;_Vector0;Vector 0;59;0;Create;True;0;0;0;False;0;False;0,1,0,1;0,1,0,1;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;113;2756.926,4051.979;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;110;2027.657,3963.517;Inherit;False;53;DetailMap_Albedo;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;120;2565.562,3984.495;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;130;2114.485,3845.844;Inherit;False;Property;_EdgeThreshold;Edge Threshold;57;0;Create;True;0;0;0;False;0;False;0.03;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;131;2734.154,3780.255;Inherit;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.TFHCRemapNode;124;2456.311,3743.46;Inherit;True;5;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;1,1,1,1;False;3;COLOR;0,0,0,0;False;4;COLOR;1,1,1,1;False;1;COLOR;0
Node;AmplifyShaderEditor.SaturateNode;127;3390.773,3707.503;Inherit;False;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;375;-8069.286,4762.156;Inherit;False;ExposureMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;354;-11923.97,3257.197;Inherit;False;Constant;_Color0;Color 0;45;0;Create;True;0;0;0;False;0;False;0,0,1,0;0,0,0,0;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;460;-9207.175,3797.779;Inherit;False;Property;_SnowMultiplier;Snow Multiplier;44;0;Create;True;0;0;0;False;0;False;1;1;0;5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;276;-9198.47,3697.499;Inherit;False;Global;GlobalSnowStrength;Global Snow Strength;41;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;249;-9525.538,3367.618;Inherit;True;True;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;232;-13785.88,3215.64;Inherit;True;Property;_SnowMap;Snow Map;42;0;Create;True;1;Detail Map;0;0;False;0;False;fd984f2b82a26e44c8d08d1cd139d097;fd984f2b82a26e44c8d08d1cd139d097;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.TFHCRemapNode;117;2274.649,3975.306;Inherit;True;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;250;-9729.996,3490.903;Inherit;False;Constant;_Float1;Float 1;56;0;Create;True;0;0;0;False;0;False;3;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;258;-7923.829,3369.955;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;48;-4454.824,217.1713;Inherit;True;Property;_DetailMap;Detail Map;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.BreakToComponentsNode;192;-3727.036,239.9353;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.RangedFloatNode;57;-3530.218,449.9033;Inherit;False;Property;_DetailNormalMapScale;Detail Normal Scale;19;0;Create;False;0;0;0;False;0;False;1;1.5;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;52;-3438.196,294.2165;Inherit;False;COLOR;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;1;COLOR;0
Node;AmplifyShaderEditor.UnpackScaleNormalNode;56;-3203.235,400.7639;Inherit;False;Tangent;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;51;-2901.712,417.7145;Inherit;False;DetailMap_Normal;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;54;-3529.989,552.0557;Inherit;False;DetailMap_Smoothness;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;201;-4290.449,479.0585;Inherit;False;188;Default;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;622;-3948.4,459.3212;Inherit;True;2;2;0;FLOAT;2;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;624;-4008.729,1211.048;Inherit;True;2;2;0;FLOAT;2;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;621;-4263.827,703.57;Inherit;False;Property;_DetailSmoothnessScale;Detail Smoothness Scale;20;0;Create;True;0;0;0;False;0;False;1;0.4;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;169;-6558.306,-5667.485;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;149;-6545.464,-5796.081;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;625;-3270.27,285.848;Inherit;False;Property;_DetailAlbedoStrength;Detail Albedo Strength;18;0;Create;True;0;0;0;False;0;False;1;1;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;626;-2991.678,185.1651;Inherit;False;3;0;FLOAT;0.5;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;628;-2914.748,930.3987;Inherit;False;3;0;FLOAT;0.5;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;59;-2705.032,953.4028;Inherit;False;DetailMap2_Albedo;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;53;-2747.594,228.1257;Inherit;False;DetailMap_Albedo;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;162;-5852.406,-5994.244;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;63;-3532.118,1181.225;Inherit;False;Property;_DetailNormalMapScale2;Detail 2 Normal Scale;24;0;Create;False;0;0;0;False;0;False;1;1;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;627;-3249.16,1032.524;Inherit;False;Property;_Detail2AlbedoStrength;Detail 2 Albedo Strength;23;0;Create;True;0;0;0;False;0;False;1;1;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;186;-4080.507,229.3608;Inherit;False;Property;_Detail;Detail;16;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;623;-4324.157,1455.297;Inherit;False;Property;_Detail2SmoothnessScale;Detail 2 Smoothness Scale;25;0;Create;True;0;0;0;False;0;False;1;0.4;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;150;1852.47,1286.503;Inherit;False;2;2;0;FLOAT;2;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;151;1458.835,1330.407;Inherit;True;54;DetailMap_Smoothness;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;632;1468.499,1568.863;Inherit;True;65;DetailMap2_Smoothness;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;631;1868.899,1533.763;Inherit;False;2;2;0;FLOAT;2;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;630;2085.999,1295.862;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;633;1748.714,1424.286;Inherit;False;21;MaskmapDetailMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;13;-1363.543,236.1694;Inherit;True;Property;_MaskMap;Mask Map;8;0;Create;False;0;0;0;False;0;False;None;2a3c69377810da645ad76e068d6e3d76;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RegisterLocalVarNode;22;-612.6275,188.583;Inherit;False;MaskmapMetallic;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;28;-461.1786,1073.63;Inherit;False;OcclusionOutput;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;19;-618.9013,267.6048;Inherit;False;MaskmapOcclusion;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;21;-617.363,352.745;Inherit;False;MaskmapDetailMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;20;-614.2063,439.5617;Inherit;False;MaskmapSmoothness;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;9;-8420.828,-5887.187;Inherit;True;Property;_BaseColorMap;Base Map;5;0;Create;False;0;0;0;False;0;False;None;b7174667adf33574abe351509b7e5fe3;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.ColorNode;11;-7157.615,-6080.819;Inherit;False;Property;_BaseColor;Base Color;4;0;Create;True;0;0;0;False;0;False;1,1,1,0;1,1,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TexturePropertyNode;41;-7935.129,-3257.718;Inherit;True;Property;_NormalMap;Normal Map;6;0;Create;False;0;0;0;False;0;False;None;902594e3384aec2419e43900bcb793cb;True;bump;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;44;-7942.589,-3013.575;Inherit;False;Property;_NormalStrength;Normal Strength;7;0;Create;True;0;0;0;False;0;False;1;1;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.UnpackScaleNormalNode;214;-14033.09,-454.3175;Inherit;False;Tangent;2;0;FLOAT4;0,0,0,0;False;1;FLOAT;0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;213;-13999.02,-295.6436;Inherit;False;287;NormalBase;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector4Node;546;-13473.4,-158.8672;Inherit;False;Property;_MossAxis;Moss Axis;39;0;Create;True;0;0;0;False;0;False;0,1,0,0;0,0,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;220;-13164.46,-489.8427;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;227;-6767.381,-238.7255;Inherit;True;MossMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;347;-12847.3,135.578;Inherit;False;Constant;_Float6;Float 6;37;0;Create;True;0;0;0;False;0;False;1;-4.1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;348;-12585.28,103.898;Inherit;False;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;224;-11622.49,57.11543;Inherit;False;28;OcclusionOutput;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;345;-12855.25,298.7838;Inherit;False;Constant;_Float5;Float 5;36;0;Create;True;0;0;0;False;0;False;2;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;350;-12586.07,343.9069;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;362;-12970.25,211.9597;Inherit;False;Property;_MossEdgeSoftness;Moss Edge Softness;33;0;Create;True;0;0;0;False;0;False;0.1;0.01;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;210;-5913.27,-1087.303;Inherit;False;MossMap_Normal;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ColorNode;269;-5433.907,-5725.67;Inherit;False;Property;_MossColor;Moss Color;27;0;Create;True;0;0;0;False;0;False;0.6941177,0.7254902,0.2235294,0;1,1,1,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;462;-8742.199,-309.0934;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;619;-9009.984,-66.46236;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;620;-8779.126,-45.10075;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;548;-8569.195,240.7307;Inherit;True;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;474;-10184.5,464.1931;Inherit;True;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;477;-10689.87,630.3566;Inherit;True;2;0;FLOAT;0;False;1;FLOAT;-1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;346;-12965.07,379.9049;Inherit;False;Property;_MossStrength;Moss Strength;29;0;Create;True;0;0;0;False;0;False;0.2705882;1.9;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;359;-12391.1,180.5597;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0.73;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;351;-12192.18,89.4871;Inherit;True;5;0;FLOAT;0;False;1;FLOAT;0.9;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;352;-11913.54,89.43906;Inherit;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;215;-13708.61,-389.6403;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;212;-14061.26,-207.5494;Inherit;False;Property;_MossNormalInfluence;Moss Normal Influence;32;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;344;-10987.54,169.6208;Inherit;False;342;AlphaMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;485;-9208.737,237.3046;Inherit;True;206;MossMap_Albedo;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;473;-10772.44,243.6388;Inherit;True;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;1;False;4;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;467;-10233.94,172.1723;Inherit;True;3;0;FLOAT;1;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;465;-11085.73,608.1666;Inherit;False;Property;_AlphaMask_Moss;Moss Mask Alpha Influence;55;0;Create;False;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;281;-630.5707,-5913.08;Inherit;False;Property;_Keyword0;Keyword 0;40;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;235;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;280;-3486.212,-5966.008;Inherit;False;Property;_Keyword0;Keyword 0;26;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Reference;245;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;245;-6891.056,-1200.563;Inherit;False;Property;_Moss;Moss;26;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;235;-11614.63,3421.358;Inherit;False;Property;_Snow;Snow;40;0;Create;True;0;0;0;False;0;False;0;0;0;True;;Toggle;2;Key0;Key1;Create;True;True;All;9;1;COLOR;0,0,0,0;False;0;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;3;COLOR;0,0,0,0;False;4;COLOR;0,0,0,0;False;5;COLOR;0,0,0,0;False;6;COLOR;0,0,0,0;False;7;COLOR;0,0,0,0;False;8;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;264;-1309.828,-5767.591;Inherit;False;Property;_SnowColor;Snow Color;41;0;Create;True;0;0;0;False;0;False;1,1,1,0;1,1,1,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;98;-2581.455,-5793.975;Inherit;False;Property;_HeightBlendColor;Height Blend Color;52;0;Create;True;0;0;0;False;0;False;0.5450981,0.6039216,0.5176471,0;0.545098,0.6039216,0.517647,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;95;-3073.871,2110.311;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.TFHCRemapNode;90;-2945.207,1875.663;Inherit;False;5;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;3;FLOAT;0;False;4;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;94;-3584.746,2197.584;Inherit;False;Property;_HeightBlendOffset;Height Blend Offset;51;0;Create;True;0;0;0;False;0;False;0;19.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;92;-3551.823,2097.56;Inherit;False;Global;GlobalHeightBlend;Global Height Blend;13;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;204;-7644.544,-1271.612;Inherit;True;Property;_MossMap;MossMap;28;0;Create;True;0;0;0;False;0;False;f64ab9c83a7958140bc3aa7ad250d7db;f64ab9c83a7958140bc3aa7ad250d7db;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;244;-6487.295,-900.0293;Inherit;False;Property;_MossNormalStrength;Moss Normal Strength;30;0;Create;True;0;0;0;False;0;False;1;1;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;49;-4743.673,214.5658;Inherit;True;Property;_DetailMap0;Detail Map;17;0;Create;False;0;0;0;False;0;False;None;89108ebb2323aff4b85461cb35684acd;False;gray;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;16;-6458.782,-5994.272;Inherit;False;2;2;0;COLOR;1,1,1,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;140;-1047.045,1083.305;Inherit;False;3;0;FLOAT;1;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;587;-695.6245,1085.026;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;635;-14335.57,1357.405;Inherit;False;Property;_AlphaMask_Moss1;Alpha Mask;56;0;Create;False;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;46;-1363.596,1205.9;Inherit;False;Property;_OcclusionStrength;Occlusion Strength;11;0;Create;True;0;0;0;False;0;False;1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;45;1478.898,1220.671;Inherit;False;Property;_Smoothness;Smoothness;10;0;Create;True;0;0;0;False;0;False;0.5;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;137;5258.77,1412.472;Inherit;False;Property;_HeightBlendSmoothness;Height Blend Smoothness;53;0;Create;True;0;0;0;False;0;False;0.35;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;336;3369.671,1440.591;Inherit;False;Property;_MossSmoothness;Moss Smoothness;31;0;Create;True;0;0;0;False;0;False;0;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;303;4203.098,1409.333;Inherit;False;Global;SnowSmoothness;Snow Smoothness;43;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;637;1025.575,-15.1043;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;0;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;638;1025.575,-15.1043;Float;False;True;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;12;VIVID Arts/Env Lit;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;18;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;12;all;0;False;True;1;1;False;;0;False;;1;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForward;False;False;0;Hidden/InternalErrorShader;0;0;Standard;38;Workflow;1;0;Surface;0;0;  Refraction Model;0;0;  Blend;0;0;Two Sided;1;0;Fragment Normal Space,InvertActionOnDeselection;0;0;Transmission;0;0;  Transmission Shadow;0.5,False,;0;Translucency;0;0;  Translucency Strength;1,False,;0;  Normal Distortion;0.5,False,;0;  Scattering;2,False,;0;  Direct;0.9,False,;0;  Ambient;0.1,False,;0;  Shadow;0.5,False,;0;Cast Shadows;1;0;  Use Shadow Threshold;0;0;Receive Shadows;1;0;GPU Instancing;1;0;LOD CrossFade;0;638093361541367231;Built-in Fog;1;0;_FinalColorxAlpha;0;0;Meta Pass;1;0;Override Baked GI;0;0;Extra Pre Pass;0;0;DOTS Instancing;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,;0;  Type;0;0;  Tess;16,False,;0;  Min;10,False,;0;  Max;25,False,;0;  Edge Length;16,False,;0;  Max Displacement;25,False,;0;Write Depth;0;0;  Early Z;0;0;Vertex Position,InvertActionOnDeselection;1;0;0;8;False;True;True;True;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;639;1025.575,-15.1043;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=ShadowCaster;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;640;1025.575,-15.1043;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;True;False;False;False;False;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;False;False;True;1;LightMode=DepthOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;641;1025.575,-15.1043;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;12;all;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;642;1025.575,-15.1043;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;12;all;0;False;True;1;1;False;;0;False;;1;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=Universal2D;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;643;1025.575,-15.1043;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthNormals;0;6;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;12;all;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;;True;3;False;;False;True;1;LightMode=DepthNormals;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;644;1025.575,-15.1043;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;GBuffer;0;7;GBuffer;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;True;0;False;;False;False;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;True;12;all;0;False;True;1;1;False;;0;False;;1;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;0;False;;255;False;;255;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;0;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalGBuffer;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;188;0;200;0
WireConnection;62;0;61;0
WireConnection;191;1;203;0
WireConnection;191;0;62;0
WireConnection;202;0;191;0
WireConnection;60;0;202;3
WireConnection;60;1;202;1
WireConnection;64;0;60;0
WireConnection;64;1;63;0
WireConnection;14;0;13;0
WireConnection;66;0;64;0
WireConnection;234;0;232;0
WireConnection;236;0;234;0
WireConnection;238;0;236;3
WireConnection;238;1;236;1
WireConnection;42;0;41;0
WireConnection;42;5;44;0
WireConnection;81;0;80;0
WireConnection;81;1;69;0
WireConnection;81;2;72;0
WireConnection;240;0;238;0
WireConnection;240;1;239;0
WireConnection;55;0;42;0
WireConnection;55;1;81;0
WireConnection;287;0;55;0
WireConnection;241;0;235;0
WireConnection;251;0;249;0
WireConnection;252;0;251;0
WireConnection;293;0;298;0
WireConnection;293;1;256;0
WireConnection;293;2;292;0
WireConnection;237;0;236;0
WireConnection;253;0;293;0
WireConnection;253;1;255;0
WireConnection;253;2;252;0
WireConnection;456;0;365;0
WireConnection;399;0;253;0
WireConnection;382;0;378;0
WireConnection;382;2;379;0
WireConnection;459;0;365;0
WireConnection;459;1;456;0
WireConnection;401;0;399;2
WireConnection;401;1;577;2
WireConnection;402;0;399;3
WireConnection;402;1;577;3
WireConnection;381;0;365;0
WireConnection;381;1;382;0
WireConnection;400;0;399;1
WireConnection;400;1;577;1
WireConnection;391;0;390;0
WireConnection;391;1;384;0
WireConnection;93;0;92;0
WireConnection;93;1;94;0
WireConnection;89;0;88;2
WireConnection;394;0;381;0
WireConnection;580;0;402;0
WireConnection;392;0;391;0
WireConnection;579;0;401;0
WireConnection;578;0;400;0
WireConnection;403;0;578;0
WireConnection;403;1;579;0
WireConnection;403;2;580;0
WireConnection;395;0;394;0
WireConnection;395;1;392;0
WireConnection;461;0;276;0
WireConnection;461;1;460;0
WireConnection;396;0;395;0
WireConnection;583;0;403;0
WireConnection;282;0;283;0
WireConnection;282;1;461;0
WireConnection;279;0;274;0
WireConnection;279;1;461;0
WireConnection;91;0;90;0
WireConnection;393;0;396;0
WireConnection;96;0;91;0
WireConnection;257;0;583;0
WireConnection;257;1;282;0
WireConnection;257;3;273;0
WireConnection;257;4;279;0
WireConnection;372;1;393;0
WireConnection;372;2;363;0
WireConnection;311;0;310;0
WireConnection;311;1;304;0
WireConnection;373;0;258;0
WireConnection;373;1;372;0
WireConnection;309;0;373;0
WireConnection;309;2;311;0
WireConnection;216;0;215;0
WireConnection;219;0;216;2
WireConnection;219;1;546;2
WireConnection;218;0;216;3
WireConnection;218;1;546;3
WireConnection;260;0;309;0
WireConnection;18;0;27;0
WireConnection;18;1;25;2
WireConnection;542;0;220;0
WireConnection;10;0;9;0
WireConnection;543;0;219;0
WireConnection;544;0;218;0
WireConnection;222;0;542;0
WireConnection;222;1;543;0
WireConnection;222;2;544;0
WireConnection;222;3;546;4
WireConnection;342;0;10;4
WireConnection;205;0;204;0
WireConnection;246;0;205;0
WireConnection;225;0;352;0
WireConnection;225;1;224;0
WireConnection;319;2;315;2
WireConnection;319;3;315;3
WireConnection;206;0;246;0
WireConnection;492;0;474;0
WireConnection;328;0;326;0
WireConnection;328;1;327;0
WireConnection;226;0;225;0
WireConnection;494;0;467;0
WireConnection;320;0;319;0
WireConnection;320;1;316;0
WireConnection;322;0;269;0
WireConnection;322;1;320;0
WireConnection;329;0;226;0
WireConnection;329;2;328;0
WireConnection;507;0;494;0
WireConnection;507;1;492;0
WireConnection;17;0;15;1
WireConnection;17;1;15;2
WireConnection;17;2;15;3
WireConnection;12;0;11;0
WireConnection;12;1;10;0
WireConnection;270;0;268;0
WireConnection;270;1;269;0
WireConnection;208;0;246;3
WireConnection;208;1;246;1
WireConnection;209;0;208;0
WireConnection;209;1;244;0
WireConnection;168;0;169;0
WireConnection;168;1;149;0
WireConnection;168;2;170;0
WireConnection;207;0;246;2
WireConnection;313;0;270;0
WireConnection;313;1;322;0
WireConnection;323;0;313;0
WireConnection;323;1;312;0
WireConnection;47;0;30;0
WireConnection;47;1;45;0
WireConnection;159;0;16;0
WireConnection;159;1;168;0
WireConnection;243;0;236;2
WireConnection;338;0;335;0
WireConnection;338;1;336;0
WireConnection;158;0;47;0
WireConnection;158;1;630;0
WireConnection;324;0;270;0
WireConnection;324;1;323;0
WireConnection;339;0;158;0
WireConnection;339;1;338;0
WireConnection;339;2;337;0
WireConnection;330;0;55;0
WireConnection;330;1;331;0
WireConnection;330;2;332;0
WireConnection;272;0;159;0
WireConnection;272;1;324;0
WireConnection;272;2;271;0
WireConnection;333;1;55;0
WireConnection;333;0;330;0
WireConnection;302;0;301;0
WireConnection;302;1;303;0
WireConnection;341;1;158;0
WireConnection;341;0;339;0
WireConnection;100;0;280;0
WireConnection;100;1;98;0
WireConnection;300;0;341;0
WireConnection;300;1;302;0
WireConnection;300;2;299;0
WireConnection;288;0;333;0
WireConnection;288;1;284;0
WireConnection;265;0;264;0
WireConnection;265;1;262;0
WireConnection;99;0;280;0
WireConnection;99;1;100;0
WireConnection;99;2;101;0
WireConnection;340;1;341;0
WireConnection;340;0;300;0
WireConnection;290;0;284;0
WireConnection;290;1;288;0
WireConnection;290;2;289;0
WireConnection;261;0;99;0
WireConnection;261;1;265;0
WireConnection;261;2;263;0
WireConnection;285;0;333;0
WireConnection;285;1;290;0
WireConnection;285;2;286;0
WireConnection;135;0;340;0
WireConnection;135;1;137;0
WireConnection;334;1;333;0
WireConnection;334;0;285;0
WireConnection;136;0;340;0
WireConnection;136;1;135;0
WireConnection;136;2;134;0
WireConnection;84;0;34;0
WireConnection;84;1;83;0
WireConnection;35;0;84;0
WireConnection;31;0;136;0
WireConnection;39;0;281;0
WireConnection;67;0;334;0
WireConnection;65;0;624;0
WireConnection;445;0;450;0
WireConnection;445;1;447;0
WireConnection;317;0;314;0
WireConnection;448;0;444;0
WireConnection;108;0;449;0
WireConnection;440;1;441;0
WireConnection;440;0;109;0
WireConnection;450;1;106;0
WireConnection;450;0;453;0
WireConnection;449;1;450;0
WireConnection;449;0;445;0
WireConnection;453;0;106;0
WireConnection;453;1;448;0
WireConnection;447;0;443;0
WireConnection;439;12;16;0
WireConnection;439;11;141;0
WireConnection;439;9;170;0
WireConnection;103;0;102;0
WireConnection;106;0;103;0
WireConnection;106;1;442;0
WireConnection;318;0;317;0
WireConnection;318;1;316;0
WireConnection;564;0;483;0
WireConnection;564;1;462;0
WireConnection;564;2;566;0
WireConnection;615;0;485;0
WireConnection;615;1;486;0
WireConnection;483;0;484;0
WireConnection;549;1;486;0
WireConnection;484;0;548;0
WireConnection;484;1;486;0
WireConnection;129;0;106;0
WireConnection;129;1;113;0
WireConnection;113;0;120;0
WireConnection;113;1;114;0
WireConnection;120;0;117;0
WireConnection;131;0;124;0
WireConnection;124;0;106;0
WireConnection;124;1;130;0
WireConnection;127;0;129;0
WireConnection;375;0;374;3
WireConnection;249;0;248;2
WireConnection;249;1;250;0
WireConnection;117;0;110;0
WireConnection;117;1;119;1
WireConnection;117;2;119;2
WireConnection;117;3;119;3
WireConnection;117;4;119;4
WireConnection;258;0;257;0
WireConnection;48;0;49;0
WireConnection;192;0;186;0
WireConnection;52;0;192;3
WireConnection;52;1;192;1
WireConnection;56;0;52;0
WireConnection;56;1;57;0
WireConnection;51;0;56;0
WireConnection;54;0;622;0
WireConnection;622;0;192;2
WireConnection;622;1;621;0
WireConnection;624;0;202;2
WireConnection;624;1;623;0
WireConnection;169;0;163;0
WireConnection;149;0;141;0
WireConnection;626;1;192;0
WireConnection;626;2;625;0
WireConnection;628;1;202;0
WireConnection;628;2;627;0
WireConnection;59;0;628;0
WireConnection;53;0;626;0
WireConnection;162;0;16;0
WireConnection;162;1;159;0
WireConnection;186;1;201;0
WireConnection;186;0;48;0
WireConnection;150;0;151;0
WireConnection;631;0;632;0
WireConnection;630;0;631;0
WireConnection;630;1;150;0
WireConnection;630;2;633;0
WireConnection;22;0;14;1
WireConnection;28;0;587;0
WireConnection;19;0;14;2
WireConnection;21;0;14;3
WireConnection;20;0;14;4
WireConnection;220;0;216;1
WireConnection;220;1;546;1
WireConnection;227;0;462;0
WireConnection;348;0;347;0
WireConnection;348;1;346;0
WireConnection;350;0;345;0
WireConnection;350;1;346;0
WireConnection;210;0;209;0
WireConnection;462;0;329;0
WireConnection;462;1;507;0
WireConnection;619;0;507;0
WireConnection;619;1;483;0
WireConnection;620;0;619;0
WireConnection;548;0;485;0
WireConnection;548;1;549;0
WireConnection;474;0;473;0
WireConnection;474;2;477;0
WireConnection;477;0;465;0
WireConnection;359;0;348;0
WireConnection;359;1;362;0
WireConnection;351;0;222;0
WireConnection;351;1;348;0
WireConnection;351;2;359;0
WireConnection;351;4;350;0
WireConnection;352;0;351;0
WireConnection;215;0;214;0
WireConnection;215;1;213;0
WireConnection;215;2;212;0
WireConnection;473;0;344;0
WireConnection;467;1;344;0
WireConnection;467;2;465;0
WireConnection;281;1;99;0
WireConnection;281;0;261;0
WireConnection;280;1;159;0
WireConnection;280;0;272;0
WireConnection;245;1;247;0
WireConnection;245;0;205;0
WireConnection;235;1;354;0
WireConnection;235;0;240;0
WireConnection;95;0;93;0
WireConnection;90;0;89;0
WireConnection;90;3;93;0
WireConnection;90;4;95;0
WireConnection;16;1;12;0
WireConnection;140;1;18;0
WireConnection;140;2;46;0
WireConnection;587;0;140;0
WireConnection;587;2;584;0
WireConnection;638;0;40;0
WireConnection;638;1;68;0
WireConnection;638;3;37;0
WireConnection;638;4;32;0
WireConnection;638;5;29;0
ASEEND*/
//CHKSM=FA097DA5D966AA32F643A20E8EF4EFD0AA01149E