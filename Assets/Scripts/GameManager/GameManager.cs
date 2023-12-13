using System.Collections.Generic;
using System.Linq;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.AI;

public class GameManager : MonoBehaviour
{
    [SerializeField] private List<GameObject> dummyList;
    [SerializeField] private GameObject enemy;
    [SerializeField] private GameObject tank;
    private GameObject[] rangeSpawnPoints;
    private GameObject[] meleeSpawnPoints;
    private bool startGame;
    private List<GameObject> enemyList;
    private FirstWave wave;

    private void Start()
    {
        startGame = false;
        rangeSpawnPoints = GameObject.FindGameObjectsWithTag("RangeSpawn");
        meleeSpawnPoints = GameObject.FindGameObjectsWithTag("MeleeSpawn");
    }

    private void Update()
    {
        if (!startGame) StartGame();
        else wave.CustomUpdate();
    }

    private void StartGame()
    {
        if (!(dummyList[0] == null && dummyList[1] == null)) return;
        startGame = true;
        wave = new FirstWave(enemy, tank, rangeSpawnPoints, meleeSpawnPoints);
    }
}
