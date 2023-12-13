using System;
using UnityEngine;
using UnityEngine.SceneManagement;

public class EndState
{
    private float time;
    public EndState(float time)
    {
        Debug.Log("Death");
        this.time = time;
    }

    public void CustomUpdate()
    {
        InitEnd();
    }

    void InitEnd()
    {
        time -= Time.deltaTime;
        if (time < 0) SceneManager.LoadScene(0);
    }
}
