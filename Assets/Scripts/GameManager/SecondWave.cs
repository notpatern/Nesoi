using UnityEngine;
using Object = UnityEngine.Object;

public class SecondWave
{
    private readonly GameObject enemy;
    private readonly GameObject tank;
    private GameObject[] rangeSpawnPoints;
    private GameObject[] meleeSpawnPoints;
    private EndState endState;
    private float time;
    public SecondWave(GameObject enemy, GameObject tank, GameObject[] rangeSpawnPoints, GameObject[] meleeSpawnPoints)
    {
        this.tank = tank;
        this.enemy = enemy;
        this.rangeSpawnPoints = rangeSpawnPoints;
        this.meleeSpawnPoints = meleeSpawnPoints;
        time = 3f;
        SpawnEnemies();
    }

    public void CustomUpdate()
    {
        if (time > 0f) CheckForWaveEnd();
        if (time < 0f) endState.CustomUpdate();
    }

    private void CheckForWaveEnd()
    {
        Debug.Log("Enemy" + GameObject.FindGameObjectsWithTag("Enemy").Length);
        Debug.Log("Melee" + GameObject.FindGameObjectsWithTag("melee").Length);
        if (GameObject.FindGameObjectsWithTag("Enemy").Length == 0 &&
            GameObject.FindGameObjectsWithTag("melee").Length == 0)
        {
            time -= Time.deltaTime;
            if (time <= 0f) NextWave();
        }
    }
    
    private void NextWave()
    {
        endState = new EndState(0.2f);
    }
    
    private void SpawnEnemies()
    {
        foreach (GameObject spawn in rangeSpawnPoints)
        {
            Object.Instantiate(enemy, spawn.transform.position, Quaternion.identity);
        }
        foreach (GameObject spawn in meleeSpawnPoints)
        {
            Object.Instantiate(tank, spawn.transform.position, Quaternion.identity);
        }
    }
}